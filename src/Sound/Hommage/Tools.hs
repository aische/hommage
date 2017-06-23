module Sound.Hommage.Tools
 ( testSound
 , testSong
 , stepSequencer
 , rhythmize
 , pitchlist
 )
 where

import Sound.Hommage.Misc
import Sound.Hommage.WavFile
import Sound.Hommage.DFTFilter
import Sound.Hommage.Signal
import Sound.Hommage.Sample

import Sound.Hommage.Midi
import Sound.Hommage.Notation
import Sound.Hommage.Seq

import Sound.Hommage.Osc
import Sound.Hommage.Envelope
import Sound.Hommage.Filter

import Sound.Hommage.Play
import Sound.Hommage.Sound
import Sound.Hommage.FFT

import Data.Time

import Control.Monad
import Data.Ratio
import System.Random
import Control.Applicative
import System.IO
import System.Process

---------------------------------------------------------------------------------------------------
-- | Usage: @ testSound mb_wavplayer outputfileprefix stereo? bpm sound @
--
--   Example: @ testSound (Just \"c:\/Programme\/coolpro\/coolpro.exe\") \"out_\" True 110.0 $ Interpolate Linear (0,1) @
testSound :: Sound a => (Maybe String) -> String -> Bool -> Double -> a -> IO ()
testSound wavplayer prefix stereo bpm a =
    do
        --let wavplayer = "c:/Programme/coolpro/coolpro.exe"
        fp <- newFilePath (\n->prefix ++ show n ++ ".wav")
        t1 <- getCurrentTime
        if stereo
            then writeWavStereo fp $ runSong bpm $ return $ playStereo a
            else writeWavMono fp $ runSong bpm $ return $ playMono a
        t2 <- getCurrentTime
        print $ diffUTCTime t2 t1
        maybe (return ()) (\wavplayer -> system (wavplayer ++ " " ++ fp) >> return ()) wavplayer


testSong :: Sound a => (Maybe String) -> String -> Bool -> Double -> Song a -> IO ()
testSong wavplayer prefix stereo bpm a =
    do
        --let wavplayer = "c:/Programme/coolpro/coolpro.exe"
        fp <- newFilePath (\n->prefix ++ show n ++ ".wav")
        t1 <- getCurrentTime
        if stereo
            then writeWavStereo fp $ runSong bpm $ fmap playStereo a
            else writeWavMono fp $ runSong bpm $ fmap playMono a
        t2 <- getCurrentTime
        print $ diffUTCTime t2 t1
        maybe (return ()) (\wavplayer -> system (wavplayer ++ " " ++ fp) >> return ()) wavplayer
---------------------------------------------------------------------------------------------------
stepSequencer :: Eq c => [(c, a)] -> Dur -> [c] -> Notation a
stepSequencer cs d = line . map (maybe (Rest d) (Note d) . flip lookup cs)
---------------------------------------------------------------------------------------------------
rhythmize :: Dur -> [Int] -> [Maybe a] -> Notation a
rhythmize dur ds as = line $
 zipWith (\d -> maybe (Rest (dur * (d % 1))) (Note (dur * (d % 1)))) ds as
---------------------------------------------------------------------------------------------------
pitchlist :: (Num a, Ord a) => Bool -> a -> [a]
pitchlist major base = takeWhile (<128) $ dropWhile (<0) $ loop $ i base
 where
  i base = if base > 0 then i (base - 12) else base
  d = if major then 4 else 3
  loop x = x : x+d : x+7 : loop (x+12)
---------------------------------------------------------------------------------------------------

