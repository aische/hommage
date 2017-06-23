{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- This file is the new version for C-code FFT algorithm with Nyquist allready on position 1.

-- | This module imports the C-functions for a Fast Fourier Transform and encapsulates them in
--   list-mapping functions. (At first it was a DFT algoritm which I changed, but I didn'd change
--   all the names of the functions, so it is called dft...)
--   The functionality of this module is used for filtering audio data, i. e. for building
--   audio filters.

module Sound.Hommage.DFTFilter
 (

-- | Filtering is done with an FFT algorithm written in C (in the file dft.c).
--   This module imports the c-functions for fft ('analyseDFT') and inverse fft ('syntheseDFT'),
--   which operate on Arrays with 1024 Double values.
--   The array-operations are embedded in a mechanism that does buffering, overlapping and windowing
--   and works like a black box that maps one input value to one output value in the IO monad
--   ('mkAnalyse', 'mkSynthese', 'mkFilterBuffered').
--   Around these IO's some list mapping functions are wrapped ('dftanalyse', 'dftsynthese', 'dftfilter',
--   'dftfilterBy') for convenient use in a Haskell programm.
--
--   Mapping the Fourier coefficients during the filter process means filtering the data.
--   The result depends on the way the coefficients are mapped resp. modified while mapping.
--   The function to do this has the type of 'CoeffMap'.
--
--   /Description of the filtering process:/
--
--   Filtering is done by mapping the coefficients with 'CoeffMap'.
--   For this the wave data is split into parts of 1024 values, where every part overlaps with the
--   preceeding and succeeding part by 512 values. Via the fourier-transform (analyse) every part
--   is mapped  to its frequency spectrum, i. e. its fourier coefficients.
--
--   The 'CoeffMap' action is applied to every frequency spectrum (see 'CoeffArr') in sequence.
--   It reads the coefficients from the first 'CoeffArr' where the input is stored and writes it
--   (or some data derived from it) to the second one. This is the point where the \'real\' filtering
--   happens. Via the inverse fourier-transform (synthese) the resulting coefficients are mapped
--   to their real signal values. These signal values are multiplied with a half cosinus curve for
--   fading in and out (windowing). Then they are mixed in the way their source data was split:
--   Every part overlaps for 512 values with the last and the next one.

 -- * User-Level Filter Functions
   dftanalyse
 , dftsynthese
 , dftfilter
 , dftfilterBy
 , dftfilterBy'

 -- * CoeffArr
 , CoeffArr
 , readCoeffArr
-- , readCoeffArr'
 , writeCoeffArr
-- , writeCoeffArr'
 , storeCoeff
 , unstoreCoeff
 -- * CoeffMap
 , CoeffMap
 , mkCoeffMap
 , coeffmap
 -- * IO Wrapper
 , mkAnalyse
 , mkSynthese
 , mkFilterBuffered
 -- * Interface to C-code
 , analyseDFT
 , syntheseDFT
 , kurveDFT
 )
 where

import GHC.Weak
import GHC.IO
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Array.Storable
import Data.Complex
import Data.Array.IO
import Data.IORef

import Sound.Hommage.Misc
---------------------------------------------------------------------------------------------------
-- ([Complex Double] -> [Complex Double]) -> IO CoeffMap

-- | (Fast) Fourier Transformation and Inverse with a buffer-mapping action for filtering.
dftfilterBy :: IO (CoeffMap ()) -> [Double] -> [Double]
dftfilterBy cm = drop 1536 . inList (mkFilterBuffered cm) . (++ replicate 512 0.0)

-- | (Fast) Fourier Transformation and Inverse with a buffer-mapping action for filtering.
dftfilterBy' :: s -> IO (s -> CoeffMap s) -> [Double] -> [Double]
dftfilterBy' s cm = drop 1536 . inList (mkFilterBuffered' s cm) . (++ replicate 512 0.0)

-- | (Fast) Fourier Transformation and Inverse with an additional argument with sublists of
--   factors to weight the coefficients. See 'mkCoeffMap' for a decription of that argument.
dftfilter :: [Double] -> [Double] -> [Double]
dftfilter ds = dftfilterBy (mkCoeffMap (replicate 512 0 ++ ds))

-- | Inverse (Fast) Fourier Transformation
dftsynthese :: [Complex Double] -> [Double]
dftsynthese = inList mkSynthese

-- | (Fast) Fourier Transformation.
dftanalyse :: [Double] -> [(Complex Double)]
dftanalyse = inList mkAnalyse

---------------------------------------------------------------------------------------------------
foreign import ccall "cinit" initfou :: Int

foreign import ccall "canalyse" cAnalyse :: Ptr Double -> Ptr Double -> IO ()

foreign import ccall "csynthese" cSynthese :: Ptr Double -> Ptr Double -> IO ()

foreign import ccall "ckurve" cKurve :: Ptr Double -> IO ()
---------------------------------------------------------------------------------------------------
-- | Maps an array with 1024 real values to its frequency spectrum with 512 complex values.
--   The spectrum is stored in the second array with 1024 Doubles which are the real and imaginary
--   parts of the complex values. See 'CoeffArr' for a detailed frequency spectrum
--   desription.
analyseDFT :: StorableArray Int Double -> StorableArray Int Double -> IO ()
analyseDFT wave coeffs = seq initfou $
 withStorableArray wave $ \ptr_w ->
 withStorableArray coeffs $ \ptr_c ->
 cAnalyse ptr_w ptr_c

-- | Maps a frequency spectrum to its signal value. Both arrays have a length of 1024; the first one
--   contains the 512 complex values of the spectrum. The real signal value will be stored in
--   the second array. See 'CoeffArr' for a detailed frequency spectrum
--   desription.
syntheseDFT :: StorableArray Int Double -> StorableArray Int Double -> IO ()
syntheseDFT coeffs wave = seq initfou $
 withStorableArray wave $ \ptr_w ->
 withStorableArray coeffs $ \ptr_c ->
 cSynthese ptr_c ptr_w

-- | Fades a signal which is stored in an array of length 1024 in and out.
--   The signal is multiplied with a cosinus curve with range 0..pi.
kurveDFT :: StorableArray Int Double -> IO ()
kurveDFT wave = seq initfou $
 withStorableArray wave $ \ptr_w ->
 cKurve ptr_w
---------------------------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------------------------
-- | Represents a frequency spectrum.
--   Range is from 0 to 1023 (N=1024).
--   Index 0 is constant value, index 1 is Nyquest frequency (real part of the N\/2 frequency).
--   Index 2 and 3 are the complex value for the basefrequency, index 4 and 5
--   are the complex value for the double basefrequency, 6 and 7 for 3 * basefrequency and so on:
--
-- > arr [0] = real part of zero frequency (imaginary part is allways 0)
-- > arr [1] = real part of Nyquest frequency (N/2) (imaginary part is allways 0)
-- > arr [2] = real part of base frequency
-- > arr [3] = imag part of base frequency
-- > arr [4] = real part of base frequency * 2
-- > arr [5] = imag part of base frequency * 2
-- > arr [6] = real part of base frequency * 3
-- > arr [7] = imag part of base frequency * 3
-- > ...
-- > arr [N-4] = real part of base frequency * (N/2 - 2)
-- > arr [N-3] = imag part of base frequency * (N/2 - 2)
-- > arr [N-2] = real part of base frequency * (N/2 - 1)
-- > arr [N-1] = imag part of base frequency * (N/2 - 1)
--
-- Pseudocode:
--
-- > c (0)   = (arr [0] :+ 0)
-- > c (512) = (arr [1] :+ 0)
--
--  and for all i=[1..511]:
--
-- > c (i) = (arr [i*2] :+ arr [i*2+1])
--
type CoeffArr = StorableArray Int Double

{-
-- | Reads a Complex value of a CoeffArr as described at 'CoeffArr'.
readCoeffArr :: CoeffArr -> Int -> IO (Complex Double)
readCoeffArr arr n = if n == 0 then readArray arr 0 >>= \x -> readArray arr 1023 >>= \y -> return (x :+ y)
                               else let j  = 2*n
                                        j' = j-1
                                    in readArray arr j' >>= \x -> readArray arr j >>= \y -> return (x :+ y)

-- | Writes a Complex value of a CoeffArr as described at 'CoeffArr'.
writeCoeffArr :: CoeffArr -> Int -> Complex Double -> IO ()
writeCoeffArr arr n c = if n == 0 then writeArray arr 0 (realPart c) >> writeArray arr 1023 (imagPart c)
                                  else let j  = 2*n
                                           j' = j-1
                                  in writeArray arr j' (realPart c) >> writeArray arr j (imagPart c)
-}

-- | Reads a Complex value of a CoeffArr (with Nyquest as imaginary part of coeff 0).
readCoeffArr :: CoeffArr -> Int -> IO (Complex Double)
readCoeffArr arr n =
 let j  = 2*n
     j' = j+1
 in readArray arr j >>= \x -> readArray arr j' >>= \y -> return (x :+ y)

-- | Writes a Complex value of a CoeffArr (with Nyquest as imaginary part of coeff 0).
writeCoeffArr :: CoeffArr -> Int -> Complex Double -> IO ()
writeCoeffArr arr n c =
 let j  = 2*n
     j' = j+1
 in writeArray arr j (realPart c) >> writeArray arr j' (imagPart c)

storeCoeff :: IOArray Int (Complex Double) -> CoeffArr -> IO ()
storeCoeff arr brr = do
 for 0 (<512) (+1) $ \i -> let i1 = i * 2
                               i2 = i1 + 1
                           in do (x :+ y) <- readArray arr i
                                 writeArray brr i1 x
                                 writeArray brr i2 y

unstoreCoeff :: CoeffArr -> IOArray Int (Complex Double) -> IO ()
unstoreCoeff brr arr = do
 for 0 (<512) (+1) $ \i -> let i1 = i * 2
                               i2 = i1 + 1
                           in do x <- readArray brr i1
                                 y <- readArray brr i2
                                 writeArray arr i (x :+ y)
---------------------------------------------------------------------------------------------------
-- | An action that maps the fourier coefficients from the first array to the second.
--   Modifying the data while mapping means filtering. See 'CoeffArr' for a description of the arrays.
type CoeffMap a = CoeffArr -> CoeffArr -> IO a

-- | Constructs a Coeffmap.
--   The sublists contain the 512 real values with which the basefrequency and the 511 complex fourier coefficients
--   are multiplied (the Nyquist frequency will be zero).
--   The first value is the factor for the constant coefficient, the second for the base frequency, the next one
--   for the double base freq and so on.
--   If the sublists have more than 512 elements, these elements are thrown away.
--   If it is shorter, the array will be filled up with zeros.
{-
mkCoeffMap :: [[Double]] -> IO (CoeffMap ())
mkCoeffMap cs = do
 r <- newIORef cs
 b <- newArray (0,511) 0.0
 return $ \a1 a2 -> let loop i (x:xs) = if i >= 512 then return () else
                                        writeArray b i x >> loop (i+1) xs
                        loop i []     = for i (<512) (+1) (\i -> writeArray b i 0.0)
                    in do xs <- readIORef r
                          let (xi,xr) | null xs   = ([], [])
                                      | otherwise = (head xs, tail xs)
                          writeIORef r xr
                          loop 0 xi
                          coeffmap b a1 a2
-}

mkCoeffMap :: [Double] -> IO (CoeffMap ())
mkCoeffMap cs = do
 r <- newIORef cs
 b <- newArray (0,511) 0.0
 return $ \a1 a2 -> let loop i (x:xs) = if i >= 512
                                         then writeIORef r (x:xs)
                                         else writeArray b i x >> loop (i+1) xs
                        loop i []     = for i (<512) (+1) (\i -> writeArray b i 0.0)
                                        >> writeIORef r []
                    in do xs <- readIORef r
                          loop 0 xs
                          coeffmap b a1 a2


-- | 'coeffmap' takes an Array with 512 values and uses them for a weigted map of the fourier
--   coefficients. The Nyquist frequency is muliplied with 0.0.
coeffmap :: StorableArray Int Double -> CoeffMap ()
coeffmap filt coeffs coeffs' = do
 for 0 (<512) (+1) (\i -> let j  = 2*i
                              j' = j + 1
                          in
                          readArray filt i >>= \c ->
                          readArray coeffs j  >>= \c1 ->
                          readArray coeffs j' >>= \c2 ->
                          writeArray coeffs' j  (c * c1) >>
                          writeArray coeffs' j' (c * c2) )
 writeArray coeffs' 1 0.0

{-
coeffmap' :: ([Complex Double] -> [Complex Double]) -> IO (CoeffMap ())
coeffmap' f = do
 let toDest arr 1024 _   = return ()
     toDest arr n (x:xs) = writeArray arr n x >> toDest arr (n+1) xs
     toDest arr n []     = writeArray arr n 0 >> toDest arr (n+1) []
 return $ \arr brr -> getElems arr >>= toDest brr 0 . f
-}
---------------------------------------------------------------------------------------------------
-- | Constructs an action that maps wave-data to coefficient-data.
--   Has a delay of 512, i. e. the first 512 elements are zero and the result has (these)
--   512 elemets more than the input.
mkAnalyse :: IO (Maybe Double -> IO (Maybe (Complex Double)))
mkAnalyse = do
 warr <- newArray (0, 1023) 0.0
 carr <- newArray (0, 1023) 0.0
 oarr <- newArray (0, 511) (0.0 :+ 0.0)
-- SIGNALSTREAM reada closea <- openSignalStream sa
 rpos <- newIORef 0
 rcnt <- newIORef Nothing
 let read ma = checkpos >> readIORef rcnt >>= maybe (more ma) rest
     more ma = maybe irest snext ma
     rest k | k <= 0    = return Nothing
            | otherwise =        do writeIORef rcnt $ Just (k-1)
                                    pos <- readIORef rpos
                                    x <- readArray oarr pos
                                    writeIORef rpos (pos + 1)
                                    return $ Just x
     irest =        do writeIORef rcnt (Just 511)
                       pos <- readIORef rpos
                       fillrest pos
                       x <- readArray oarr pos
                       writeIORef rpos (pos + 1)
                       return $ Just x
     snext a =        do pos <- readIORef rpos
                         x <- readArray oarr pos
                         writeArray warr (pos+512) a
                         writeIORef rpos (pos + 1)
                         return $ Just x
     checkpos = readIORef rpos >>= \p -> if p < 512 then return () else do
                writeIORef rpos 0
                analyseDFT warr carr
                unstoreCoeff carr oarr
                for 0 (<512) (+1) (\i -> readArray warr (i+512) >>= writeArray warr i)
     fillrest k = for (512+k) (<1024) (+1) (\i -> writeArray warr i 0.0)
 return read
---------------------------------------------------------------------------------------------------
-- | Constructs an action that maps coefficient-data to wave-data.
--   Has a delay of 512, i. e. the first 512 elements are zero and the result has (these)
--   512 elemets more than the input.
mkSynthese :: IO (Maybe (Complex Double) -> IO (Maybe Double))
mkSynthese = do
 iarr <- newArray (0, 511) (0.0 :+ 0.0)
 carr <- newArray (0, 1023) 0.0
 warr <- newArray (0, 1023) 0.0
 (oarr :: IOArray Int Double) <- newArray (0, 511) 0.0
-- SIGNALSTREAM reada closea <- openSignalStream sa
 rpos <- newIORef 0
 rcnt <- newIORef Nothing
 let read ma = checkpos >> readIORef rcnt >>= maybe (more ma) rest
     more ma = maybe irest snext ma
     rest k | k <= 0    = return Nothing
            | otherwise =        do writeIORef rcnt $ Just (k-1)
                                    pos <- readIORef rpos
                                    x <- readArray oarr pos
                                    writeIORef rpos (pos + 1)
                                    return $ Just x
     irest =        do writeIORef rcnt (Just 511)
                       pos <- readIORef rpos
                       fillrest pos
                       x <- readArray oarr pos
                       writeIORef rpos (pos + 1)
                       return $ Just x
     snext a =        do pos <- readIORef rpos
                         x <- readArray oarr pos
                         writeArray iarr pos a
                         writeIORef rpos (pos + 1)
                         return $ Just x
     checkpos = readIORef rpos >>= \p -> if p < 512 then return () else do
                writeIORef rpos 0
                storeCoeff iarr carr
                for 0 (<512) (+1) (\i -> readArray warr (i+512) >>= writeArray oarr i)
                syntheseDFT carr warr
                kurveDFT warr
                for 0 (<512) (+1) (\i -> readArray warr i >>= \x ->
                                         readArray oarr i >>= \y ->  writeArray oarr i (x+y))
     fillrest k = for k (<512) (+1) (\i -> writeArray iarr i (0.0 :+ 0.0))
 return read
---------------------------------------------------------------------------------------------------
-- | Constructs an action that maps wave-data to wave-data via a Fast Foutrier Transform and inverse,
--   filtered by the given 'CoeffMap'.
--   Has a delay of 1024.
mkFilterBuffered :: IO (CoeffMap ()) -> IO (Maybe Double -> IO (Maybe Double))
mkFilterBuffered mkf = do
 f <- mkf
 warr <- newArray (0, 1023) 0.0
 carr <- newArray (0, 1023) 0.0
 carr' <- newArray (0, 1023) 0.0
 warr' <- newArray (0, 1023) 0.0
 (oarr :: IOArray Int Double) <- newArray (0, 511) 0.0
-- SIGNALSTREAM reada closea <- openSignalStream sa
 rpos <- newIORef 0
 rcnt <- newIORef Nothing
 let read ma = checkpos >> readIORef rcnt >>= maybe (more ma) rest
     more ma = maybe irest snext ma
     rest k | k <= 0    = return Nothing
            | otherwise =        do writeIORef rcnt $ Just (k-1)
                                    pos <- readIORef rpos
                                    x <- readArray oarr pos
                                    writeIORef rpos (pos + 1)
                                    return $ Just x
     irest =        do writeIORef rcnt (Just 511)
                       pos <- readIORef rpos
                       fillrest pos
                       x <- readArray oarr pos
                       writeIORef rpos (pos + 1)
                       return $ Just x
     snext a =        do pos <- readIORef rpos
                         x <- readArray oarr pos
                         writeArray warr (pos+512) a
                         writeIORef rpos (pos + 1)
                         return $ Just x
     checkpos = readIORef rpos >>= \p -> if p < 512 then return () else do
                writeIORef rpos 0
                analyseDFT warr carr
                for 0 (<512) (+1) (\i -> readArray warr (i+512) >>= writeArray warr i >>
                                         readArray warr' (i+512) >>= writeArray oarr i)
                f carr carr'
                syntheseDFT carr' warr'
                kurveDFT warr'
                for 0 (<512) (+1) (\i -> readArray warr' i >>= \x ->
                                         readArray oarr i >>= \y ->  writeArray oarr i (x+y))

     fillrest k = for (512+k) (<1024) (+1) (\i -> writeArray warr i 0.0)
 return read
---------------------------------------------------------------------------------------------------

-- | Constructs an action that maps wave-data to wave-data via a Fast Foutrier Transform and inverse,
--   filtered by the given 'CoeffMap'.
--   Has a delay of 1024.
mkFilterBuffered' :: s -> IO (s -> CoeffMap s) -> IO (Maybe Double -> IO (Maybe Double))
mkFilterBuffered' st mkf = do
 stref <- newIORef st
 f <- mkf
 warr <- newArray (0, 1023) 0.0
 carr <- newArray (0, 1023) 0.0
 carr' <- newArray (0, 1023) 0.0
 warr' <- newArray (0, 1023) 0.0
 (oarr :: IOArray Int Double) <- newArray (0, 511) 0.0
-- SIGNALSTREAM reada closea <- openSignalStream sa
 rpos <- newIORef 0
 rcnt <- newIORef Nothing
 let read ma = checkpos >> readIORef rcnt >>= maybe (more ma) rest
     more ma = maybe irest snext ma
     rest k | k <= 0    = return Nothing
            | otherwise =        do writeIORef rcnt $ Just (k-1)
                                    pos <- readIORef rpos
                                    x <- readArray oarr pos
                                    writeIORef rpos (pos + 1)
                                    return $ Just x
     irest =        do writeIORef rcnt (Just 511)
                       pos <- readIORef rpos
                       fillrest pos
                       x <- readArray oarr pos
                       writeIORef rpos (pos + 1)
                       return $ Just x
     snext a =        do pos <- readIORef rpos
                         x <- readArray oarr pos
                         writeArray warr (pos+512) a
                         writeIORef rpos (pos + 1)
                         return $ Just x
     checkpos = readIORef rpos >>= \p -> if p < 512 then return () else do
                writeIORef rpos 0
                analyseDFT warr carr
                for 0 (<512) (+1) (\i -> readArray warr (i+512) >>= writeArray warr i >>
                                         readArray warr' (i+512) >>= writeArray oarr i)
                s <- readIORef stref
                s' <- f s carr carr'
                writeIORef stref s'
                syntheseDFT carr' warr'
                kurveDFT warr'
                for 0 (<512) (+1) (\i -> readArray warr' i >>= \x ->
                                         readArray oarr i >>= \y ->  writeArray oarr i (x+y))

     fillrest k = for (512+k) (<1024) (+1) (\i -> writeArray warr i 0.0)
 return read
---------------------------------------------------------------------------------------------------















{-
-- | Reads a Complex value of a CoeffArr
readCoeffArr :: CoeffArr -> Int -> IO (Complex Double)
readCoeffArr arr n = if n == 0 then readArray arr 0 >>= \x -> readArray arr 1023 >>= \y -> return (x :+ y)
                               else let j  = 2*n
                                        j' = j-1
                                    in readArray arr j' >>= \x -> readArray arr j >>= \y -> return (x :+ y)

-- | Writes a Complex value of a CoeffArr
writeCoeffArr :: CoeffArr -> Int -> Complex Double -> IO ()
writeCoeffArr arr n c = if n == 0 then writeArray arr 0 (realPart c) >> writeArray arr 1023 (imagPart c)
                                  else let j  = 2*n
                                           j' = j-1
                                  in writeArray arr j' (realPart c) >> writeArray arr j (imagPart c)

-- | Copies coefficient data from an IOArray with Complex values (512 elements) to a
--   StorableArray with Doubles (1024 elements)
storeCoeff :: IOArray Int (Complex Double) -> CoeffArr -> IO ()
storeCoeff arr brr = do
 for 1 (<512) (+1) $ \i -> let i2 = i * 2
                               i1 = i2 - 1
                           in do (x :+ y) <- readArray arr i
                                 writeArray brr i1 x
                                 writeArray brr i2 y
 (x :+ y) <- readArray arr 0
 writeArray brr 0 x
 writeArray brr 1023 y

-- | Copies coefficient data from a StorableArray with Doubles (1024 elements) to
--   an IOArray with Complex values (512 elements).
unstoreCoeff :: CoeffArr -> IOArray Int (Complex Double) -> IO ()
unstoreCoeff brr arr = do
 for 1 (<512) (+1) $ \i -> let i2 = i * 2
                               i1 = i2 - 1
                           in do x <- readArray brr i1
                                 y <- readArray brr i2
                                 writeArray arr i (x :+ y)
 x <- readArray brr 0
 y <- readArray brr 1023
 writeArray arr 0 (x :+ y)

coeffmap :: StorableArray Int Double -> CoeffMap ()
coeffmap filt coeffs coeffs' = do
 for 1 (<512) (+1) (\i -> let j' = 2*i
                              j  = j' - 1
                          in
                          readArray filt i >>= \c ->
                          readArray coeffs j  >>= \c1 ->
                          readArray coeffs j' >>= \c2 ->
                          writeArray coeffs' j  (c * c1) >>
                          writeArray coeffs' j' (c * c2) )
 readArray filt 0 >>= \c -> readArray coeffs 0 >>= \c' -> writeArray coeffs' 0 (c * c')
 writeArray coeffs' 1023 0.0
-}
