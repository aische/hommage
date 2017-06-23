-- | Dieses Module stellt Datentypen und Funktionen zum Umgang mit Mono- und
--   Stereodaten zur Verfügung. Speziell im Falle des Öffnens einer WAV-Datei
--   ist erst zur Laufzeit bekannt, ob es sich um eine Mono- oder Stereodatei
--   handelt, hier wird der Typ 'Signal' benötigt.
module Sound.Hommage.Signal
 (
 -- * Mono and Stereo Values
   Mono
 , Stereo (..)
 , leftStereo
 , rightStereo
 , stereoToMono
 , monoToStereo
 , balance
 -- * Signal datatype
 , Signal (..)
 , readWavSignal
 , openWavSignal
 , writeWavMono
 , writeWavStereo
 , writeWavSignal

 , writeTracks

 , signalToMono
 , signalToStereo
 , eitherSignal
 , liftSignal
 , mergeSignal
 , mergeSignals
 , multSignal
 , infiniteSignal
 )
 where

import Sound.Hommage.WavFile
import Sound.Hommage.Misc

import System.IO.Unsafe
import Data.List
import Data.Typeable
-------------------------------------------------------------------------------
-- | A mono value.
type Mono = Double
-------------------------------------------------------------------------------
-- | A stereo value with left and right part. It is an instance of class Num
--   and Fractional.
data Stereo = !Double :><: !Double
 deriving (Eq, Show)

-- | Access to the left part of a 'Stereo' value.
leftStereo :: Stereo -> Mono
leftStereo (x :><: _) = x

-- | Access to the right part of a 'Stereo' value.
rightStereo :: Stereo -> Mono
rightStereo (_ :><: x) = x

instance Num Stereo where
 (l1:><:r1) + (l2:><:r2) = (l1+l2) :><: (r1+r2)
 (l1:><:r1) - (l2:><:r2) = (l1-l2) :><: (r1-r2)
 (l1:><:r1) * (l2:><:r2) = (l1*l2) :><: (r1*r2)
 negate (l:><:r) = negate l :><: negate r
 abs (l:><:r) = abs l :><: abs r
 signum (l:><:r) = signum l :><: signum r
 fromInteger i = fromInteger i :><: fromInteger i

instance Fractional Stereo where
 (l1:><:r1) / (l2:><:r2) = (l1/l2) :><: (r1/r2)
 fromRational x = fromRational x :><: fromRational x
-------------------------------------------------------------------------------
-- | Converts a Stereo value to a Mono value (Double)
stereoToMono :: Stereo -> Mono
stereoToMono (x:><:y) =  (x+y) / 2.0

-- | Converts a Mono value (Double) to a Stereo value
monoToStereo :: Mono -> Stereo
monoToStereo x = x :><: x

--panorama :: Double -> Mono -> Stereo
--panorama p m | p == 0.0 = m :><: m
--             | p >  0.0 = m :><: ((1.0  - p) * m)
--             | p <  0.0 = m :><: ((1.0  + p) * m)

-- | The range of the Double value must be between -1 and 1. If it is below 0
--   the left channel is turned down, if it is greater than 0 the right channel
--   is turned down. NOTE: This function should be replaced by a better one.
balance :: Double -> Stereo -> Stereo
balance p (l :><: r) | p == 0.0 = l :><: r
                     | p >  0.0 = l :><: ((1.0  - p) * r)
                     | p <  0.0 = ((1.0  + p) * l) :><: r
-------------------------------------------------------------------------------
-- | A Signal is either a list of 'Mono' values or a list of 'Stereo' values.
data Signal = Mono [Mono]
            | Stereo [Stereo]
  deriving Typeable

eitherSignal :: ([Mono] -> a) -> ([Stereo] -> a) -> Signal -> a
eitherSignal f g (Mono xs)   = f xs
eitherSignal f g (Stereo xs) = g xs

-- | Applies the function to the input signal. If it is a stereo signal, the
--   function is applied to both channels seperately.
liftSignal :: ([Mono] -> [Mono]) -> Signal -> Signal
liftSignal f (Mono xs)   = Mono $ f xs
liftSignal f (Stereo xs) = Stereo $ zipWith (:><:)
                           (f $ map leftStereo xs) (f $ map rightStereo xs)

-- mapSignal, liftSignal,
-------------------------------------------------------------------------------
-- | Reads a 'Signal' from a WAV-File.
readWavSignal :: FilePath -> IO Signal
readWavSignal fp = readWavFile fp >>=
 return . either
  (Mono . map wavInt16ToDouble)
  (Stereo . map (\(l,r) -> (wavInt16ToDouble l :><: wavInt16ToDouble r)))

-- | Opens a 'Signal' from a WAV-File.
openWavSignal :: FilePath -> Signal
openWavSignal fp = unsafePerformIO $ readWavSignal fp

-- | Writes a list of mono values to a WAV-File.
writeWavMono :: FilePath -> [Mono] -> IO ()
writeWavMono fp = writeWavFileMono fp . map wavDoubleToInt16

-- | Writes a list of stereo values to a WAV-File.
writeWavStereo :: FilePath -> [Stereo] -> IO ()
writeWavStereo fp = writeWavFileStereo fp . map
 (\(l :><: r) -> (wavDoubleToInt16 l, wavDoubleToInt16 r))

-- | Writes a 'Signal' to a WAV-File.
writeWavSignal :: FilePath -> Signal -> IO ()
writeWavSignal fp (Mono xs)   = writeWavMono fp xs
writeWavSignal fp (Stereo xs) = writeWavStereo fp xs

writeTracks :: FilePath -> [Signal] -> IO ()
writeTracks fp s = writeWavFiles fp "" strs
 where
  strs = map foo s
  foo (Mono xs) = Left $ map wavDoubleToInt16 xs
  foo (Stereo xs) = Right $ map (\(l:><:r)->(wavDoubleToInt16 l, wavDoubleToInt16 r)) xs
-------------------------------------------------------------------------------
-- | Transfroms a signal to a list of mono values.
signalToMono :: Signal -> [Mono]
signalToMono (Mono xs) = xs
signalToMono (Stereo xs) = map stereoToMono xs

-- | Transfroms a signal to a list of stereo values.
signalToStereo :: Signal -> [Stereo]
signalToStereo (Mono xs) = map monoToStereo xs
signalToStereo (Stereo xs) = xs
-------------------------------------------------------------------------------
-- | The sum of a set of signals. If all signals are mono, the result will be mono.
--   Otherwise it will be stereo.
mergeSignals :: [Signal] -> Signal
mergeSignals ss = fun
 where
  loop (Mono   x : xs) = let (l,r) = loop xs in (x:l,r)
  loop (Stereo x : xs) = let (l,r) = loop xs in (l,x:r)
  loop []             = ([],[])
  fun = case loop ss of
         ([],[]) -> Mono []
         (ls,[]) -> Mono $ map sum $ transpose ls
         ([],rs) -> Stereo $ map sum $ transpose rs
         (ls,rs) -> Stereo $ map sum $ transpose (map (monoToStereo . sum) (transpose ls) : rs)

-- | The sum of two signals
mergeSignal :: Signal -> Signal -> Signal
mergeSignal (Stereo xs) (Stereo ys) = Stereo $ merge (+) xs ys
mergeSignal (Stereo xs) (Mono ys)   = Stereo $ merge (+) xs (map monoToStereo ys)
mergeSignal (Mono xs)   (Stereo ys) = Stereo $ merge (+) (map monoToStereo xs) ys
mergeSignal (Mono xs)   (Mono ys)   = Mono $ merge (+) xs ys

-- | The Double value is added to the input signal (offset). The resulting signal will be infinie
--   in any case.
infiniteSignal :: Double -> Signal -> Signal
infiniteSignal d (Mono a)   = Mono (map (+d) a ++ repeat d)
infiniteSignal d (Stereo a) = Stereo (map (+(d:><:d)) a ++ repeat (d:><:d))

-- | Multiplies two signals.
multSignal :: Signal -> Signal -> Signal
multSignal (Mono a)   (Mono b)   = Mono $ zipWith (*) a b
multSignal (Mono a)   (Stereo b) = Stereo $ zipWith (\x y -> monoToStereo x * y) a b
multSignal (Stereo a) (Stereo b) = Stereo $ zipWith (*) a b
multSignal (Stereo a) (Mono b)   = Stereo $ zipWith (\x y -> x * monoToStereo y) a b
