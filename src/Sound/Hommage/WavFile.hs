{-# LANGUAGE FlexibleContexts #-}
module Sound.Hommage.WavFile
 (
 -- * Binary Files
   writeDataFile
 , readDataFile
 , openDataFile

 -- * WAV-Files
 , writeWavFile
 , writeWavFileMono
 , writeWavFileStereo
 , readWavFile
 , openWavFile

 , writeWavFiles

 -- * Cast between Int16 and Double representation of WAV-data
 , wavInt16ToDouble
 , wavDoubleToInt16

 -- * Low-Level implemetation
 -- ** Arrays and Files
 , readArrayFromFile
 , writeArrayToFileWithHeader
 , writeArrayToFile

 -- ** Single Stream
 , openSingleInputFile
 , openSingleOutputFile

 , openSingleInputWavFile
 , openSingleOutputWavFileMono
 , openSingleOutputWavFileStereo

 -- ** Buffered Stream
 , openInputFile
 , openOutputFile
 , openOutputFileWithHeader
 , openInputWavFile
 , openOutputWavFileMono
 , openOutputWavFileStereo

 -- ** Header Stuff and others
 , HeaderFun
 , HeaderSize
 , noHeader

 , wavHeaderFunMono
 , wavHeaderFunStereo
 , wavHeaderSize

 , initWriteWavHeaderMono
 , initWriteWavHeaderStereo
 , initReadWavHeader
 , closeWriteWavHeader
 , encode
 , decode
 , encodeWavLengt
 , initWavHeaderMono
 , initWavHeaderStereo
 , sizeOfArrayElements
 , inferSizeOfArrayElements
 , inferSizeOfArrayElements'
 )
 where

import System.IO
import Control.Monad
import Control.Monad.Fix
import Data.Char
import Data.Int
import GHC.IO.Handle
import GHC.IO
import GHC.Base hiding (mapM)
import Data.Complex

import Foreign.Storable
import Data.Array.Storable
import Data.Array.IArray --(bounds)
import Data.IORef

import System.IO.Unsafe
--import System.IO (hGetChar, hPutChar, hGetBuf, hPutBuf)
import GHC.Weak


---------------------------------------------------------------------------------------------------
wavInt16ToDouble :: Int16 -> Double
wavInt16ToDouble i = (fromIntegral i) / 32768.0

wavDoubleToInt16 :: Double -> Int16
wavDoubleToInt16 d = round (d * 32767.0)
---------------------------------------------------------------------------------------------------
readDataFile :: Storable a => FilePath -> IO [a]
readDataFile fp = init
 where
  init = do (c,r,_) <- openSingleInputFile 10000 0 fp
            mkWeak r () (Just c)
            loop r
  loop r = r >>= maybe (return []) (\a -> return (a : unsafePerformIO (loop r)))

openDataFile :: Storable a => FilePath -> [a]
openDataFile fp = unsafePerformIO $ readDataFile fp

writeDataFile :: Storable a => FilePath -> [a] -> IO ()
writeDataFile fp xs = do
 (c,w) <- openSingleOutputFile 4000 fp
 let loop (x:r) = w x >> loop r
     loop []    = c
 loop xs

---------------------------------------------------------------------------------------------------
readWavFile :: FilePath -> IO (Either [Int16] [(Int16,Int16)])
readWavFile fp = {- putStrLn ("opening " ++ fp) >> -} init
 where
  init = do (c,r,_) <- openSingleInputWavFile 100000 fp
            either (\rm -> do mkWeak rm () (Just c) --(putStrLn ("close " ++ fp) >> c))
                              fmap Left $ loop rm)
                   (\rs -> do mkWeak rs () (Just c) --(putStrLn ("close " ++ fp) >> c))
                              fmap Right $ loop rs)
                   r
  loop r = r >>= maybe (return []) (\a -> return (a : unsafePerformIO (loop r)))

openWavFile :: FilePath -> Either [Int16] [(Int16,Int16)]
openWavFile fp = unsafePerformIO $ readWavFile fp

writeWavFile :: FilePath -> Either [Int16] [(Int16, Int16)] -> IO ()
writeWavFile fp (Left w)  = writeWavFileMono fp w
writeWavFile fp (Right w) = writeWavFileStereo fp w

writeWavFileMono :: FilePath -> [Int16] -> IO ()
writeWavFileMono fp xs = do
 (c,w) <- openSingleOutputWavFileMono 100000 fp
 let loop (x:r) = w x >> loop r
     loop []    = c
 loop xs

writeWavFileStereo :: FilePath -> [(Int16,Int16)] -> IO ()
writeWavFileStereo fp xs = do
 (c,w) <- openSingleOutputWavFileStereo 100000 fp
 let loop (x:r) = w x >> loop r
     loop []    = c
 loop xs

writeWavFiles :: FilePath -> String  -> [Either [Int16] [(Int16, Int16)]] -> IO ()
writeWavFiles fp fn sns = do
  let init count (Left x : xs) = do (c,w) <- openSingleOutputWavFileMono 100000 (fp ++ fn ++ "_" ++ show count ++ ".wav")
                                    r <- init (count+1) xs
                                    return (Left (c,w,x) : r)
      init count (Right x : xs) = do (c,w) <- openSingleOutputWavFileStereo 100000 (fp ++ fn ++ "_" ++ show count ++ ".wav")
                                     r <- init (count+1) xs
                                     return (Right (c,w,x) : r)
      init _ _ = return []
  ss <- init 0 sns
  let step (Left (c,w,x:xs) : r) = do w x
                                      r <- step r
                                      return (Left (c,w,xs) : r)
      step (Left (c,w,[]) : r)   = c >> step r
      step (Right (c,w,x:xs) : r) = do w x
                                       r <- step r
                                       return (Right (c,w,xs) : r)
      step (Right (c,w,[]) : r)   = c >> step r
      step []                     = return []

      loop [] = return ()
      loop xs = step xs >>= loop
  loop ss
---------------------------------------------------------------------------------------------------
-- Header

-- | The first Action is applied after opening the file. Then the data
--   bytes are written. The second Action is then called with the number of
--   bytes of the data. It must close the handle.
type HeaderFun = (Handle -> IO (), Handle -> Int -> IO ())
type HeaderSize = Int

noHeader :: HeaderFun
noHeader = (const $ return (), \h _ -> return ())

-------------------------------------------------
-- WavHeader

-- | The first Action writes the Header of the wav-file.
--   Then the wav-data is written. The second action moves
--   the Handle to the positions where the lenght of the wav-file
--   are encoded (in the header) and writes the right number, which
--   is unknown before all data is written.
--   Afterwards it closes the file.
wavHeaderFunMono :: HeaderFun
wavHeaderFunMono = (initWriteWavHeaderMono, closeWriteWavHeader)

wavHeaderFunStereo :: HeaderFun
wavHeaderFunStereo = (initWriteWavHeaderStereo, closeWriteWavHeader)

wavHeaderSize :: HeaderSize
wavHeaderSize = 44
-------------------------------------------------
initWriteWavHeaderMono :: Handle -> IO ()
initWriteWavHeaderMono h = mapM_ (hPutChar h) initWavHeaderMono

initWriteWavHeaderStereo :: Handle -> IO ()
initWriteWavHeaderStereo h = mapM_ (hPutChar h) initWavHeaderStereo

initReadWavHeader :: Handle -> IO (Maybe (Bool, Int, Int, Int))
initReadWavHeader handle =
 let check [] = return True
     check (x:xs) = hGetChar handle >>= \y -> if x /= ord y then return False else check xs
     m >>? n = m >>= \b -> if b then n else return Nothing
 in hSeek handle AbsoluteSeek 0 >>
    check [82,73,70,70] >>?
    (replicateM 4 (hGetChar handle) >>= \l1 ->
    check [87, 65, 86, 69, 102,109,116,32, 16, 0,  0,  0, 1,  0] >>?
    (hGetChar handle >>= \s1 ->
    check [0, 68, 172, 0, 0] >>?
    (replicateM 4 (hGetChar handle) >>= \x1 ->
    (hGetChar handle >>= \s2 ->
    check [0,  16, 0, 100, 97,116,97] >>?
    (replicateM 4 (hGetChar handle) >>= \l2 ->
    let b | ord s1 == 1 && ord s2 == 2 = False
          | ord s1 == 2 && ord s2 == 4 = True
          | otherwise          = error ("invalid wav file " ++ show s1 ++ ", " ++ show s2)
    in return $ Just (b, decode l1, decode l2, decode x1) ) ) ) ) )

closeWriteWavHeader :: Handle -> Int -> IO ()
closeWriteWavHeader h len = do
 let (sa,sb) = encodeWavLengt len
 hSeek h AbsoluteSeek 4
 mapM (hPutChar h) sa
 hSeek h AbsoluteSeek 40
 mapM (hPutChar h) sb
 hClose h

encode :: Int -> String
encode a = map chr
 [ mod a 256
 , mod (div a 256) 256
 , mod (div a 65536) 256
 , mod (div a 16777216) 256
 ]

decode :: String -> Int
decode [a0,a1,a2,a3] = sum
 [ord a0, 256 * ord a1, 65536 * ord a2, 16777216 * ord a3]


encodeWavLengt :: Int -> (String, String)
encodeWavLengt len = (encode (len + 8), encode len)

initWavHeaderMono :: [Char]
initWavHeaderMono =
 map chr
 [82,73,70,70,
  0,0,0,0,
  87, 65, 86, 69,
  102,109,116,32,
  16, 0,  0,  0,
  1,  0,  1,  0,
  68, 172,0,  0,
  136,88, 1,  0,
  2,  0,  16, 0,
  100, 97,116,97,
  0,0,0,0]

initWavHeaderStereo :: [Char]
initWavHeaderStereo =
 map chr
 [82,73,70,70,
  0,0,0,0,
  87, 65, 86, 69,
  102,109,116,32,
  16, 0,  0,  0,
  1,  0,  2,  0,
  68, 172,0,  0,
  16,177, 2,  0,
  4,  0,  16, 0,
  100, 97,116,97,
  0,0,0,0]

---------------------------------------------------------------------------------------------------
sizeOfArrayElements :: Storable a => StorableArray Int a -> Int
sizeOfArrayElements x = sizeOf (foo x)
 where
  foo :: StorableArray Int a -> a
  foo x = undefined

inferSizeOfArrayElements :: Storable a => (StorableArray Int a -> IO Int) -> StorableArray Int a -> Int
inferSizeOfArrayElements _ arr = sizeOfArrayElements arr

inferSizeOfArrayElements' :: Storable a => (StorableArray Int a -> Int -> IO ()) -> StorableArray Int a -> Int
inferSizeOfArrayElements' _ arr = sizeOfArrayElements arr

inferSizeOfArrayElements'' :: Storable a => (StorableArray Int a -> Int -> IO Int) -> StorableArray Int a -> Int
inferSizeOfArrayElements'' _ arr = sizeOfArrayElements arr

---------------------------------------------------------------------------------------------------
-- Array / File
---------------------------------------------------------------------------------------------------
readArrayFromFile :: Storable a => HeaderSize -> FilePath -> IO (StorableArray Int a, Int)
readArrayFromFile offset filepath = do
 handle <- openBinaryFile filepath ReadMode
 hSeek handle AbsoluteSeek $ fromIntegral offset
 bytes <- hFileSize handle
 let bytesize = fromIntegral bytes - offset
 arr <- mfix $ \arr -> do let bufsize = div bytesize (sizeOfArrayElements arr)
                          newArray_ (0, bufsize-1)
 withStorableArray arr $ \ptr -> hGetBuf handle ptr bytesize
 hClose handle
 return (arr, div bytesize (sizeOfArrayElements arr))

writeArrayToFileWithHeader :: (MArray StorableArray a IO, Storable a) => HeaderFun -> FilePath -> StorableArray Int a -> IO ()
writeArrayToFileWithHeader (initH, closeH) filepath arr = do
 handle <- openBinaryFile filepath WriteMode
 initH handle
 (0, n) <- getBounds arr
 let elemsize = sizeOfArrayElements arr
     bytesize = (1+n) * elemsize
 withStorableArray arr (\ptr -> hPutBuf handle ptr bytesize)
 closeH handle bytesize

writeArrayToFile :: (MArray StorableArray a IO, Storable a) => FilePath -> StorableArray Int a -> IO ()
writeArrayToFile filepath arr = do
 handle <- openBinaryFile filepath WriteMode
 (0, n) <- getBounds arr
 let elemsize = sizeOfArrayElements arr
     bytesize = (1+n) * elemsize
 withStorableArray arr (\ptr -> hPutBuf handle ptr bytesize)
 hClose handle

{-
writeArrayToFileWithHeader :: Storable a => HeaderFun -> FilePath -> StorableArray Int a -> IO ()
writeArrayToFileWithHeader (initH, closeH) filepath arr = do
 handle <- openBinaryFile filepath WriteMode
 initH handle
 let (0,n) = bounds arr
     elemsize = sizeOfArrayElements arr
     bytesize = (1+n) * elemsize
 withStorableArray arr (\ptr -> hPutBuf handle ptr bytesize)
 closeH handle bytesize

writeArrayToFile :: Storable a => FilePath -> StorableArray Int a -> IO ()
writeArrayToFile filepath arr = do
 handle <- openBinaryFile filepath WriteMode
 let (0,n) = bounds arr
     elemsize = sizeOfArrayElements arr
     bytesize = (1+n) * elemsize
 withStorableArray arr (\ptr -> hPutBuf handle ptr bytesize)
 hClose handle
-}
---------------------------------------------------------------------------------------------------
-- Single Wav
---------------------------------------------------------------------------------------------------
openSingleInputWavFile :: Int -> FilePath -> IO (IO (), Either (IO (Maybe Int16)) (IO (Maybe (Int16,Int16))), Int)
openSingleInputWavFile n filepath = do
 handle <- openBinaryFile filepath ReadMode
 initReadWavHeader handle >>= maybe (error "unknown wav format") (\(isstereo, len1, len2, x) -> do
  hSeek handle AbsoluteSeek 44
  let stereo = do let bufsize = n*2
                  rpos   <- newIORef bufsize
                  rmax   <- newIORef bufsize
                  buf    <- newArray_ (0, bufsize-1)
                  let elemsize = sizeOfArrayElements buf
                      bytesize = elemsize * bufsize
                      next = readIORef rpos >>= \pos ->
                             readIORef rmax >>= \max ->
                             if pos >= max
                              then if max < bufsize
                                    then return Nothing
                                    else withStorableArray buf (\ptr -> hGetBuf handle ptr bytesize) >>= \bts ->
                                         writeIORef rmax (div bts elemsize) >>
                                         writeIORef rpos 0 >>
                                         next
                              else readArray buf pos >>= \v1 ->
                                   readArray buf (pos+1) >>= \v2 ->
                                   writeIORef rpos (pos + 2) >>
                                   return (Just (v1,v2))
                      close = hClose handle
                  return (close, Right next,div len2 4)
      mono   = do let bufsize = n
                  rpos   <- newIORef bufsize
                  rmax   <- newIORef bufsize
                  buf    <- newArray_ (0, bufsize-1)
                  let elemsize = sizeOfArrayElements buf
                      bytesize = elemsize * bufsize
                      next = readIORef rpos >>= \pos ->
                             readIORef rmax >>= \max ->
                             if pos >= max
                              then if max < bufsize
                                    then return Nothing
                                    else withStorableArray buf (\ptr -> hGetBuf handle ptr bytesize) >>= \bts ->
                                         writeIORef rmax (div bts elemsize) >>
                                         writeIORef rpos 0 >>
                                         next
                              else readArray buf pos >>= \v ->
                                   writeIORef rpos (pos + 1) >>
                                   return (Just v)
                      close = hClose handle
                  return (close, Left next,div len2 2)
  if isstereo then stereo else mono )
---------------------------------------------------------------------------------------------------
openSingleOutputWavFileMono :: Int -> FilePath -> IO (IO (), Int16 -> IO ())
openSingleOutputWavFileMono bufsize filepath = do
 handle <- openBinaryFile filepath WriteMode
 rpos   <- newIORef 0
 buf    <- newArray_ (0, bufsize-1)
 rcnt   <- newIORef 0
 let elemsize = sizeOfArrayElements buf
     bytesize = elemsize * bufsize
     write v = readIORef rpos >>= \pos ->
               if pos >= bufsize
                then withStorableArray buf (\ptr -> hPutBuf handle ptr bytesize) >>
                     modifyIORef rcnt (+bytesize) >>
                     writeIORef rpos 1 >> writeArray buf 0 v
                else writeArray buf pos v >> writeIORef rpos (pos+1)
     close = readIORef rpos >>= \pos ->
             withStorableArray buf (\ptr -> hPutBuf handle ptr (pos*elemsize)) >>
             readIORef rcnt >>= \cnt -> closeWriteWavHeader handle (elemsize*pos+cnt)
 initWriteWavHeaderMono handle
 return (close, write)
---------------------------------------------------------------------------------------------------
openSingleOutputWavFileStereo :: Int -> FilePath -> IO (IO (), (Int16, Int16) -> IO ())
openSingleOutputWavFileStereo bufsizearg filepath = do
 let bufsize | bufsizearg < 2  = 4
             | even bufsizearg = bufsizearg
             | otherwise       = bufsizearg + 1
 handle <- openBinaryFile filepath WriteMode
 rpos   <- newIORef 0
 buf    <- newArray_ (0, bufsize-1)
 rcnt   <- newIORef 0
 let elemsize = sizeOfArrayElements buf
     bytesize = elemsize * bufsize
     write (v1,v2) = readIORef rpos >>= \pos ->
                     if pos >= bufsize
                      then withStorableArray buf (\ptr -> hPutBuf handle ptr bytesize) >>
                           modifyIORef rcnt (+bytesize) >>
                           writeIORef rpos 2 >> writeArray buf 0 v1 >> writeArray buf 1 v2
                      else writeArray buf pos v1 >> writeArray buf (pos+1) v2 >> writeIORef rpos (pos+2)
     close = readIORef rpos >>= \pos ->
             withStorableArray buf (\ptr -> hPutBuf handle ptr (pos*elemsize)) >>
             readIORef rcnt >>= \cnt -> closeWriteWavHeader handle (elemsize*pos+cnt)
 initWriteWavHeaderStereo handle
 return (close, write)
---------------------------------------------------------------------------------------------------
-- Single Storable
---------------------------------------------------------------------------------------------------
openSingleInputFile :: Storable a => Int -> HeaderSize -> FilePath -> IO (IO (), IO (Maybe a), Int)
openSingleInputFile bufsize offset filepath = do
 handle <- openBinaryFile filepath ReadMode
 bytes <- hFileSize handle
 hSeek handle AbsoluteSeek $ fromIntegral offset
 rpos   <- newIORef bufsize
 rmax   <- newIORef bufsize
 buf    <- newArray_ (0, bufsize-1)
 let elemsize = sizeOfArrayElements buf
     bytesize = elemsize * bufsize
     next = readIORef rpos >>= \pos ->
            readIORef rmax >>= \max ->
            if pos >= max
             then if max < bufsize
                   then return Nothing
                   else withStorableArray buf (\ptr -> hGetBuf handle ptr bytesize) >>= \bts ->
                        writeIORef rmax (div bts elemsize) >>
                        writeIORef rpos 0 >>
                        next
             else readArray buf pos >>= \v ->
                  writeIORef rpos (pos + 1) >>
                  return (Just v)
     close = hClose handle
 return (close, next,div (fromIntegral bytes - offset) elemsize)
---------------------------------------------------------------------------------------------------
openSingleOutputFile :: Storable a => Int -> FilePath -> IO (IO (), a -> IO ())
openSingleOutputFile bufsize filepath = do
 handle <- openBinaryFile filepath WriteMode
 rpos   <- newIORef 0
 buf    <- newArray_ (0, bufsize-1)
 let elemsize = sizeOfArrayElements buf
     bytesize = elemsize * bufsize
     write v = readIORef rpos >>= \pos ->
               if pos >= bufsize
                then withStorableArray buf (\ptr -> hPutBuf handle ptr bytesize) >>
                     writeIORef rpos 1 >> writeArray buf 0 v
                else writeArray buf pos v >> writeIORef rpos (pos+1)
     close = readIORef rpos >>= \pos ->
             withStorableArray buf (\ptr -> hPutBuf handle ptr (pos*elemsize)) >>
             hClose handle
 return (close, write)
---------------------------------------------------------------------------------------------------
-- Buffered Storable
---------------------------------------------------------------------------------------------------
openInputFile :: Storable a => HeaderSize -> FilePath -> Int -> IO (IO (), StorableArray Int a -> Int -> IO Int, Int)
openInputFile offset filepath arrsize = do
 handle <- openBinaryFile filepath ReadMode
 hSeek handle AbsoluteSeek $ fromIntegral offset
 bytes <- hFileSize handle
 let nr_bytes  = fromIntegral bytes - offset
     elemsize  = inferSizeOfArrayElements'' read undefined
     read arr k = withStorableArray arr $ \ptr ->
                  hGetBuf handle ptr (k * elemsize) >>= return . flip div elemsize
     close     = hClose handle
 return $ (close, read, div nr_bytes elemsize)

openOutputFile :: Storable a => FilePath -> IO (IO (), StorableArray Int a -> Int -> IO ())
openOutputFile filepath = do
 handle <- openBinaryFile filepath WriteMode
 let elemsize = inferSizeOfArrayElements' write undefined
     write arr k = withStorableArray arr (\ptr -> hPutBuf handle ptr (k*elemsize))
     close = hClose handle
 return $ (close, write)

openOutputFileWithHeader :: Storable a => HeaderFun -> FilePath -> IO (IO (), StorableArray Int a -> Int -> IO ())
openOutputFileWithHeader (initH, closeH) filepath = do
 handle <- openBinaryFile filepath WriteMode
 rlen   <- newIORef 0
 initH handle
 let elemsize = inferSizeOfArrayElements' write undefined
     write arr k = modifyIORef rlen (+k) >> withStorableArray arr (\ptr -> hPutBuf handle ptr (k*elemsize))
     close =  readIORef rlen >>= \l -> closeH handle (elemsize*l)
 return $ (close, write)
---------------------------------------------------------------------------------------------------
-- Buffered Wav
---------------------------------------------------------------------------------------------------

openInputWavFile :: FilePath -> IO (IO (), StorableArray Int Int16 -> Int -> IO (), Int, Bool)
openInputWavFile filepath = do
 handle <- openBinaryFile filepath ReadMode
 initReadWavHeader handle >>= maybe (error "unknown wav format") (\(isstereo, len1, len2, x) -> do
  hSeek handle AbsoluteSeek 44
  let elemsize  = inferSizeOfArrayElements' read undefined
      read arr k = withStorableArray arr $ \ptr ->
                   hGetBuf handle ptr (k * elemsize) >> return ()
      close     = hClose handle
  return $ (close, read, div len2 2, isstereo) )

openOutputWavFileMono :: FilePath -> IO (IO (), StorableArray Int Int16 -> Int -> IO ())
openOutputWavFileMono filepath = do
 handle <- openBinaryFile filepath WriteMode
 rlen   <- newIORef 0
 let elemsize = inferSizeOfArrayElements' write undefined
     write arr k = withStorableArray arr (\ptr -> hPutBuf handle ptr (k*elemsize)) >> modifyIORef rlen (+k)
     close = readIORef rlen >>= \l -> closeWriteWavHeader handle (elemsize*l)
 initWriteWavHeaderMono handle
 return $ (close, write)

openOutputWavFileStereo :: FilePath -> IO (IO (), StorableArray Int Int16 -> Int -> IO ())
openOutputWavFileStereo filepath = do
 handle <- openBinaryFile filepath WriteMode
 rlen   <- newIORef 0
 let elemsize = inferSizeOfArrayElements' write undefined
     write arr k = withStorableArray arr (\ptr -> hPutBuf handle ptr (k*elemsize)) >> modifyIORef rlen (+k)
     close = readIORef rlen >>= \l -> closeWriteWavHeader handle (elemsize*l)
 initWriteWavHeaderStereo handle
 return $ (close, write)
---------------------------------------------------------------------------------------------------
