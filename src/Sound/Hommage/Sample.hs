{-# LANGUAGE FlexibleContexts #-}
module Sound.Hommage.Sample
 (
 -- * Sample Datastructure
   Sample
 , arraySample
 , sizeSample
 , newSample
 , newSampleFromList

 -- * File Operations
 , openWavSample
 , saveWavSampleMono
 , saveWavSampleStereo

 -- * Pitching, Playing and Scratching
 , pitchWavSignal
 , scratchWavSignal
 , playSample
 , pitchSample
 , scratchSample
 , playCoeffs

 -- * Transforming Samples
 , mapSample
 , foldlSample
 , updateSample
 , normaliseSample
 , fadeinoutSample
 , stretchSample

 -- * Filter and Fourier Coeffs
 , Coeffs
 , arrayCoeffs
 , sizeCoeffs
 , wavesizeCoeffs
 , syntheseSample
 , analyseSample
 , filterCoeffs
 , mkPlayCoeffs
 , readCoeffs
 , writeCoeffs

 -- * Playing a Sample
 , mkTriggerSample
 , mkPlaySample
 , mkScratchSample
 , mkPitchSample

 -- * Array functions
 , interpolArray
 , mkLoopArray
 , mkPlayArray

 )
 where

import Foreign.Storable
import Data.Array.Storable
import Data.Array.IO
import Data.Int
import Data.IORef
import Data.Complex

import Sound.Hommage.Misc
import Sound.Hommage.WavFile
import Sound.Hommage.DFTFilter
import Sound.Hommage.Signal

---------------------------------------------------------------------------------------------------
-- | Open a Wav-File, but play it with variable (positive) speed\/ frequency.
pitchWavSignal :: FilePath -> [Double] -> Signal
pitchWavSignal fp = either Mono Stereo . inListE''
 (openWavSample fp >>= either (fmap Left . mkPitchSample) (fmap Right . mkPitchSample) )

-- | Open a Wav-File, but play it with variable (positive and\/or negative) speed\/ frequency.
--   Backward playing is possible.
scratchWavSignal :: FilePath -> [Double] -> Signal
scratchWavSignal fp = either Mono Stereo . inListE'
 (openWavSample fp >>= either (fmap Left . mkScratchSample) (fmap Right . mkScratchSample) )
---------------------------------------------------------------------------------------------------
-- | Creates a finite list with the content of the Sample.
playSample :: Sample a -> [a]
playSample = toList' . mkPlaySample

-- | Creates a list with the content of the Sample, but played with variable speed\/frequency.
pitchSample :: Fractional a => Sample a -> [Double] -> [a]
pitchSample s = inList'' $ mkPitchSample s

-- | Creates a list with the content of the Sample, but played with variable speed\/frequency,
--   backward playing possible.
scratchSample :: Fractional a => Sample a -> [Double] -> [a]
scratchSample s = inList' $ mkScratchSample s

-- | Creates a list with the content of 'Coeffs' (a representation of the fourier coefficients of
--   a 'Sample').
playCoeffs :: Coeffs -> [Complex Double]
playCoeffs = toList' . mkPlayCoeffs
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
--interpolArray :: MArray array Double IO => array Int Double -> Int -> Double -> IO Double
interpolArray :: (Fractional a, MArray array a IO) => array Int a -> Int -> Double -> IO a
interpolArray arr size =
 let f  = fromRational . toRational
 in \pos -> do
  let p0 = floor pos
      p1 = p0 + 1
      d :: Double
      d  = pos - fromIntegral p0
      d' = 1.0 - d
  v1 <- readArray arr $ mod p0 size
  v2 <- readArray arr $ mod p1 size
  let r = v1 * f d' + v2 * f d
  return r

mkLoopArray :: (Fractional a, MArray arr a IO) => arr Int a -> Int -> IO (Double -> IO a)
mkLoopArray arr size = do
 r <- newIORef 0.0
 let f  = fromRational . toRational
 return $ \dp ->
  do pos <- readIORef r
     let p  = floor pos
         p0 = mod p size
         p1 = mod (p + 1) size
         d :: Double
         d  = pos - fromIntegral p
         pos' = d + fromIntegral p0
         d' = 1.0 - d
     v1 <- readArray arr p0
     v2 <- readArray arr p1
     writeIORef r (pos' + dp)
     return (v1 * f d' + v2 * f d)

mkPlayArray :: (Fractional a, MArray arr a IO) => arr Int a -> Int -> IO (Double -> IO (Maybe a))
mkPlayArray arr size = do
 r <- newIORef 0.0
 let f  = fromRational . toRational
 return $ \dp ->
  do pos <- readIORef r
     let p0 = floor pos
         p1 = p0 + 1
         d :: Double
         d  = pos - fromIntegral p0
         d' = 1.0 - d
     if p1 >= size then return Nothing else do
      v1 <- readArray arr p0
      v2 <- readArray arr p1
      writeIORef r (pos + abs dp)
      return $ Just (v1 * f d' + v2 * f d)


---------------------------------------------------------------------------------------------------
data Sample a = SAMPLE
 { arraySample :: !(IOArray Int a)
 , sizeSample  :: !Int
 }
---------------------------------------------------------------------------------------------------
newSample :: Int -> a -> IO (Sample a)
newSample n a = newArray (0, n-1) a >>= \arr -> return $ SAMPLE arr n

newSampleFromList :: [a] -> IO (Sample a)
newSampleFromList xs = do
 let n = length xs
 arr <- newListArray (0, n - 1) xs
 return $ SAMPLE arr n

mapSample :: (a -> b) -> Sample a -> IO (Sample b)
mapSample f (SAMPLE arr n) = do
 brr <- newArray_ (0, n - 1)
 for 0 (<n) (+1) (\i -> readArray arr i >>= writeArray brr i . f)
 return $ SAMPLE brr n
---------------------------------------------------------------------------------------------------
openWavSample :: FilePath -> IO (Either (Sample Mono) (Sample Stereo))
openWavSample filepath = do
 (close, read, len, stereo) <- openInputWavFile filepath
 let bsize = 4000
 buf <- newArray_ (0, bsize-1)
 if stereo
  then do let lenh = div len 2
              hsize = div bsize 2
          arr <- newArray_ (0, lenh - 1)
          let step o i = do let i1=i*2                  -- i: maximal 1999
                            x1 <- readArray buf i1      --
                            x2 <- readArray buf (i1+1)
                            writeArray arr (o + i) (wavInt16ToDouble x1 :><: wavInt16ToDouble x2)
              loop o k | k < bsize = do read buf k
                                        for 0 (<(div k 2)) (+1) (step o)
                                        close
                                        return $ Right $ SAMPLE arr lenh
                       | otherwise = do read buf bsize
                                        for 0 (<hsize) (+1) (step o)
                                        loop (o+hsize) (k-bsize)
          loop 0 len
  else do arr <- newArray_ (0, len - 1)
          let loop o k | k < bsize = do read buf k
                                        for 0 (<k) (+1) (\i -> readArray buf i >>= writeArray arr (o + i) . wavInt16ToDouble )
                                        close
                                        return $ Left $ SAMPLE arr len
                       | otherwise = do read buf bsize
                                        for 0 (<bsize) (+1) (\i -> readArray buf i >>= writeArray arr (o + i) . wavInt16ToDouble )
                                        loop (o+bsize) (k-bsize)
          loop 0 len
---------------------------------------------------------------------------------------------------
saveWavSampleMono :: FilePath -> Sample Double -> IO ()
saveWavSampleMono filepath (SAMPLE arr len) = do
 (close, write) <- openOutputWavFileMono filepath
 let bsize = 4000
 buf <- newArray_ (0, bsize-1)
 let step o i = do x <- readArray arr (o+i)
                   writeArray buf i (wavDoubleToInt16 x)
     loop o k | k < bsize = do for 0 (<k) (+1) (step o)
                               write buf k
                               close
              | otherwise = do for 0 (<bsize) (+1) (step o)
                               write buf bsize
                               loop (o + bsize) (k - bsize)
 loop 0 len
---------------------------------------------------------------------------------------------------
saveWavSampleStereo :: FilePath -> Sample Stereo -> IO ()
saveWavSampleStereo filepath (SAMPLE arr len) = do
 (close, write) <- openOutputWavFileStereo filepath
 let bsize = 4000
     hsize = div bsize 2
 buf <- newArray_ (0, bsize-1)
 let step o i = do let i1 = i * 2
                       i2 = i1 + 1
                   x <- readArray arr (o+i)
                   writeArray buf i1 (wavDoubleToInt16 $ leftStereo x)
                   writeArray buf i2 (wavDoubleToInt16 $ rightStereo x)
     loop o k | k < hsize = do for 0 (<k) (+1) (step o)
                               write buf (2*k)
                               close
              | otherwise = do for 0 (<hsize) (+1) (step o)
                               write buf bsize
                               loop (o + hsize) (k - hsize)
 loop 0 len

--12000    348


---------------------------------------------------------------------------------------------------
mkTriggerSample :: Sample a -> a -> IO (Bool -> IO a)
mkTriggerSample s a = do
 rp <- newIORef 0
 let n = sizeSample s
     arr = arraySample s
     fun b = if b
              then readIORef rp >>= \p ->
                   if p < n
                    then writeIORef rp (p+1) >>
                         readArray arr p
                    else return a
              else writeIORef rp 0 >>
                   return a
 return fun

mkPlaySample :: Sample a -> IO (IO (Maybe a))
mkPlaySample (SAMPLE arr n) = do
 rpos <- newIORef 0
 let read = readIORef rpos >>= \pos ->
            if pos >= n then return Nothing
                        else writeIORef rpos (pos+1) >> readArray arr pos >>= return . Just
 return read
---------------------------------------------------------------------------------------------------
foldlSample :: Sample a -> (b -> a -> b) -> b -> IO b
foldlSample (SAMPLE arr n) f x =
 for' 0 (<n) (+1) x (\i x -> readArray arr i >>= \y -> return (f x y))

updateSample :: Sample a -> (a -> a) -> IO ()
updateSample (SAMPLE arr n) f =
 for 0 (<n) (+1) (\i -> readArray arr i >>= writeArray arr i . f)
---------------------------------------------------------------------------------------------------
normaliseSample :: (Ord a, Fractional a) => Sample a -> IO ()
normaliseSample s = do
 m <- foldlSample s (\b a -> max b (abs a)) 0.001
 updateSample s ((*)(1.0 / m))
---------------------------------------------------------------------------------------------------
fadeinoutSample :: Fractional a => (Int, Int) -> Sample a -> IO ()
fadeinoutSample (li,lo) sa = do
 let n = sizeSample sa
     arr = arraySample sa
     li' = min li n
     lo' = min lo n
     ol  = n - lo'
     si  = 1.0 / fromIntegral li'
     so  = 1.0 / fromIntegral lo'
 for' 0 (<li') (+1) 0.0 (\i s -> readArray arr i >>= \v -> writeArray arr i (v * s) >> return (s+si))
 for' ol (<n) (+1) 1.0 (\i s -> readArray arr i >>= \v -> writeArray arr i (v * s) >> return (s-so))
 return ()

---------------------------------------------------------------------------------------------------
stretchSample :: Fractional a => Int -> Sample a -> IO (Sample a)
stretchSample n sam = do
 let s   = sizeSample sam
     arr = arraySample sam
     d   = fromIntegral s / fromIntegral n
 ram <- newSample n 0.0
 for 0 (<n) (+1) (\i -> interpolArray arr s (d * fromIntegral i) >>= writeArray (arraySample ram) i)
 return ram

mkScratchSample :: Fractional a => Sample a -> IO (Double -> IO a)
mkScratchSample (SAMPLE arr n) = mkLoopArray arr n

mkPitchSample :: Fractional a => Sample a -> IO (Double -> IO (Maybe a))
mkPitchSample (SAMPLE arr n) = mkPlayArray arr n
---------------------------------------------------------------------------------------------------
data Coeffs = COEFFS
 { arrayCoeffs    :: IOArray Int CoeffArr
 , sizeCoeffs     :: Int
 , wavesizeCoeffs :: Int
 }

mkPlayCoeffs :: Coeffs -> IO (IO (Maybe (Complex Double)))
mkPlayCoeffs (COEFFS arr siz wsiz) = do
 r <- newIORef (0,0)
 let read = readIORef r >>= \(n,k) ->
            if n >= siz then return Nothing else
            if k >= 512 then writeIORef r (n+1,0) >> read else
            readArray arr n >>= \cs ->
            readCoeffArr cs k >>= \c ->
            writeIORef r (n,k+1) >>
            return (Just c)
 return read

analyseSample :: Sample Mono -> IO Coeffs
analyseSample s = do
 let arr = arraySample s
     siz   = sizeSample s
     n     = div siz 512
     r     = mod siz 512
     n'    = if r == 0 then n-1 else n
     n''   = n' - 1
     r'    = if r == 0 then 1024 else r+512
     p'    = n'' * 512
 dummy <- newArray_ (0,1023)
 brr <- newArray (0, n'') undefined
 for 0 (<n'') (+1) (\i -> do let p = i * 512
                             for 0 (<1024) (+1) (\i -> readArray arr (i+p) >>= writeArray dummy i)
                             crr <- newArray_ (0, 1023)
                             writeArray brr i crr
                             analyseDFT dummy crr)
 crr <- newArray_ (0, 1023)
 writeArray brr n'' crr
 for 0 (<r') (+1) (\i -> readArray arr (i+p') >>= writeArray dummy i)
 for r' (<1024) (+1) (\i -> writeArray dummy i 0.0)
 analyseDFT dummy crr
 return $ COEFFS brr n' siz

syntheseSample :: Coeffs -> IO (Sample Mono)
syntheseSample (COEFFS arr n siz) = do
 so <- newSample siz 0.0
 let n' = n-1
 dummy <- newArray (0, 1023) 0.0
-- sa :: StoraArray Int Double) <- newArray (0, 1023) 0.0
 for 0 (<n') (+1) (\i -> do crr <- readArray arr i
                            syntheseDFT crr dummy
                            kurveDFT dummy
                            let pl = i * 512
                            for 0 (<512) (+1) (\j -> let jpl = j + pl
                                                         jpr = jpl + 512
                                                     in
                                                     readArray (arraySample so) jpl >>= \x ->
                                                     readArray dummy j >>= \y ->
                                                     writeArray (arraySample so) jpl (x+y) >>
                                                     readArray dummy (j+512) >>= \z ->
                                                     writeArray (arraySample so) jpr z
                                              )
                  )
 crr <- readArray arr n'
 syntheseDFT crr dummy
 kurveDFT dummy
 let pl = n' * 512
     pm = siz - (pl + 512)
 for 0 (<512) (+1) (\j -> let jpl = j + pl
                          in
                          readArray (arraySample so) jpl >>= \x ->
                          readArray dummy j >>= \y ->
                          writeArray (arraySample so) jpl (x+y) )
 for 0 (<pm) (+1) (\j -> readArray dummy (j+512) >>= \z ->
                          writeArray (arraySample so) (j+pl+512) z )
 return so

filterCoeffs :: IO (CoeffMap ()) -> Coeffs -> IO Coeffs
filterCoeffs mkf (COEFFS arrci n siz) = do
 arrco <- newArray (0, n-1) undefined
 f   <- mkf
 for 0 (<n) (+1) (\i -> do ci <- readArray arrci i
                           co <- newArray_ (0, 1023)
                           f ci co
                           writeArray arrco i co)
 return $ COEFFS arrco n siz

-- | usage: @ readCoeffs obj nrOfCoeffarr nrOfCoefficient @
readCoeffs :: Coeffs -> Int -> Int -> IO (Complex Double)
readCoeffs cf a c = readArray (arrayCoeffs cf) a >>= \ca ->
                    readCoeffArr ca c

writeCoeffs :: Coeffs -> Int -> Int -> (Complex Double) -> IO ()
writeCoeffs cf a c v = readArray (arrayCoeffs cf) a >>= \ca ->
                       writeCoeffArr ca c v

---------------------------------------------------------------------------------------------------






