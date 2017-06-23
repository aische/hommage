{-# LANGUAGE FlexibleInstances #-}
module Sound.Hommage.Sound
 (
 -- * Classes
   (==>)
 , (.=>)
 , Sound (..)
 , Effect (..)
 , (<+>)
 , (<*>)

 -- * Sound Generators and Transformers
 , WaveForm (..)
 , playWaveForm
 , Oscillator (..)
 , ToFreq (..)
 , ToFreqDyn (..)
 , Noise (..)
 , PlayWav (..)
 , OscWav (..)
 , PitchWav (..)
 , ScratchWav (..)
 , Filter (..)
 , FilterFFT (..)
 , Lowpass (..)
 , Highpass (..)
 , Bandpass (..)
 , Morphfilter (..)
 , Stretchfilter (..)

 , Envelope (..)
 , Amplifier (..)
 , AddTo (..)
 , Infinite (..)
 , Delay (..)
 , ToMono (..)
 , Average (..)
 , SampleAndHold (..)
 , Compressor (..)
 , Follow (..)
 , Panorama (..)

-- , LowpassRect (..)

 )
 where

import Data.List
import Data.Ratio
--import Control.Applicative hiding ((<*>))
import Prelude hiding ((<*>))
--import Control.Monad.Reader

import Sound.Hommage.Signal
import Sound.Hommage.Envelope
import Sound.Hommage.Notation
import Sound.Hommage.Play
import Sound.Hommage.Osc
import Sound.Hommage.Filter
import Sound.Hommage.Sample
import Sound.Hommage.DFTFilter
import Sound.Hommage.FFT

-----------------------------------------------------------
(==>) :: (Sound a, Effect b) => a -> b -> Play Signal
v ==> e = play  v >>= \s -> effect e >>= \f -> return (f s)

(<+>) :: (Sound a, Sound b) => a -> b -> Play Signal
a <+> b = play a >>= \s1 -> play b >>= \s2 -> return (mergeSignal s1 s2)

(<*>) :: (Sound a, Sound b) => a -> b -> Play Signal
a <*> b = play a >>= \s1 -> play b >>= \s2 -> return (multSignal s1 s2)
-----------------------------------------------------------
-- | Minimal complete definition: 'play'.
class Sound a where
 play :: a -> Play Signal
 playMono   :: a -> Play [Mono]
 playStereo :: a -> Play [Stereo]

 playMono a = play a >>= return . signalToMono
 playStereo a = play a >>= return . signalToStereo

instance Sound () where
 play = const $ return (Mono [])

instance Sound (Play Signal) where
 play = id

instance Sound (Play [Mono]) where
 play a = a >>= return . Mono

instance Sound (Play [Stereo]) where
 play a = a >>= return . Stereo

instance Sound Signal where
 play = return

instance Sound [Mono] where
 play = return . Mono

instance Sound [Stereo] where
 play = return . Stereo

instance Sound Mono where
 play = return . Mono . repeat

instance Sound Stereo where
 play = return . Stereo . repeat

--instance Sound FilePath where
-- play = return . openWavSignal

instance Sound (Track Signal) where
 play = playTrack

instance Sound (Track [Mono]) where
 play = fmap Mono . playTrack

instance Sound (Track [Stereo]) where
 play = fmap Stereo . playTrack

instance Sound Interpolate where
 play i = getDur >>= \d -> return $ Mono $ toEnv i (absDur d)
-----------------------------------------------------------
class Effect a where
 effect :: a -> Play (Signal -> Signal)
 effectMono :: a -> Play ([Mono] -> [Mono])
 effectStereo :: a -> Play ([Stereo] -> [Stereo])

 effectMono a = effect a >>= \f -> return ( signalToMono . f . Mono )
 effectStereo a = effect a >>= \f -> return ( signalToStereo . f . Stereo )

(.=>) :: (Effect a, Effect b) => a -> b -> Play (Signal -> Signal)
a .=> b = effect a >>= \fa -> effect b >>= \fb -> return (fb . fa)


instance Sound a => Effect (Play Signal -> a) where
 effect f = PLAY $ \d e s -> unPlay (play $ f $ return s) d e

instance Sound a => Effect (Play [Mono] -> a) where
 effect f = PLAY $ \d e s -> unPlay (play $ f $ return $ signalToMono s) d e

instance Sound a => Effect (Play [Stereo] -> a) where
 effect f = PLAY $ \d e s -> unPlay (play $ f $ return $ signalToStereo s) d e

instance Effect (Play (Signal -> Signal)) where
 effect = id

--instance Effect (Play Signal -> Play Signal) where
-- effect f = PLAY $ \d e s -> unPlay (f (return s)) d e

--instance Sound a => Effect (Signal -> a) where
-- effect f = PLAY $ \e d s -> unPlay (play $ f s) e d

instance Effect (Signal -> Signal) where
 effect = return

instance Effect ([Mono] -> [Mono]) where
 effect = return . liftSignal
 effectMono = return

instance Effect ([Stereo] -> [Stereo]) where
 effectStereo = return
 effect f = return (Stereo . f . signalToStereo)

--instance Effect ([Mono] -> [Stereo]) where
-- effect f = return $ eitherSignal (Stereo . f) (Stereo . id) -- ???

--instance Effect ([Stereo] -> [Mono]) where
-- effect f = return $ eitherSignal (Mono . id) (Mono . f) -- ???

--instance Effect ([Mono] -> Signal) where
-- effect f = return $ eitherSignal f Stereo  -- ???

--instance Effect ([Stereo] -> Signal) where
-- effect f = return $ eitherSignal Mono f  -- ???
-----------------------------------------------------------
newtype PlayWav = PlayWav FilePath

instance Sound PlayWav where
 play (PlayWav fp) = return $ openWavSignal fp
-----------------------------------------------------------
data Filter a = Filter a

instance Sound a => Effect (Filter a) where
 effect (Filter a) = playMono a >>= \a' -> return $ liftSignal (dftfilter a')

data FilterFFT a = FilterFFT Int a

instance Sound a => Effect (FilterFFT a) where
 effect (FilterFFT n a) = playMono a >>= \a' -> return $ liftSignal (ffttv n a')

-----------------------------------------------------------
-- | Width and cutoff.
data Lowpass width cutoff = Lowpass width cutoff

instance (Sound a, Sound b) => Sound (Lowpass a b) where
 play (Lowpass a b) = playMono a >>= \a -> playMono b >>= \b -> return $ Mono (lowpass a b)
-----------------------------------------------------------
-- | Width and cutoff.
data Highpass width cutoff = Highpass width cutoff

instance (Sound a, Sound b) => Sound (Highpass a b) where
 play (Highpass a b) = playMono a >>= \a -> playMono b >>= \b -> return $ Mono (highpass a b)
-----------------------------------------------------------
data Amplifier volume = Amplifier volume

instance Sound a => Effect (Amplifier a) where
 effect (Amplifier a) = play a >>= return . multSignal
---------------------------------------------------------------------------------------------------
data Envelope = Envelope EnvMode EnvShape ADSR
 deriving (Eq, Read, Show)

instance Sound Envelope where
 play (Envelope em ec adsr) = getDur >>= return . Mono . playADSR em ec adsr . absDur

instance Sound [(Env, EnvLength)] where
 play e = getDur >>= \d -> return $ Mono $ runEnv e (absDur d)
-----------------------------------------------------------
data AddTo summand = AddTo summand

instance Sound a => Effect (AddTo a) where
 effect (AddTo a) = play a >>= \s -> return (mergeSignal s)
-----------------------------------------------------------
-- | The result will be infinite, with given offset.
data Infinite = Infinite Double

instance Effect Infinite where
 effect (Infinite d) = return $ infiniteSignal d
-----------------------------------------------------------
data Noise = Noise

instance Sound Noise where
 play _ = return $ Mono $ randomList (-1.0,1.0)
 playStereo _ = return $ zipWith (:><:) (randomList (-1.0,1.0)) (randomList (-1.0,1.0))
-----------------------------------------------------------
-- | Turns pitch to frequency.
data ToFreq = ToFreq Double

instance Effect ToFreq where
 effect (ToFreq d) = return $ liftSignal (map (noteToFrequency d))


data ToFreqDyn a = ToFreqDyn a

instance Sound a => Effect (ToFreqDyn a) where
 effect (ToFreqDyn a) = playMono a >>= \s -> return $ liftSignal (zipWith noteToFrequency s)

 -----------------------------------------------------------
data WaveForm = Sinus | Cosinus | Rect | Saw | Tri
 deriving (Eq, Read, Show)

playWaveForm :: WaveForm -> [Double] -> [Double]
playWaveForm wf =
 case wf of
  Sinus   -> sinus
  Cosinus -> cosinus
  Rect    -> rect
  Saw     -> saw
  Tri     -> tri

instance Effect WaveForm where
 effect wf =  case wf of
  Sinus   -> return (liftSignal sinus)
  Cosinus -> return (liftSignal cosinus)
  Rect    -> return (liftSignal rect)
  Saw     -> return (liftSignal saw)
  Tri     -> return (liftSignal tri)
-----------------------------------------------------------
data Oscillator = Oscillator WaveForm Double

instance Effect Oscillator where
 effect (Oscillator wf d) = return $
  (Mono . playWaveForm wf . map (adjustFrequency 1024 d) . signalToMono)
-----------------------------------------------------------
data OscWav = OscWav FilePath Double

instance Effect OscWav where
 effect (OscWav fp d) = let w = signalToMono $ openWavSignal fp in
                            return (Mono . osc (cycle w) .
                                     map (adjustFrequency (fromIntegral $ length w) d) .
                                    signalToMono)
-----------------------------------------------------------
data PitchWav = PitchWav FilePath

instance Effect PitchWav where
 effect (PitchWav fp) = return $ pitchWavSignal fp . signalToMono
-----------------------------------------------------------
data ScratchWav = ScratchWav FilePath

instance Effect ScratchWav where
 effect (ScratchWav fp) = return $ scratchWavSignal fp . signalToMono
-----------------------------------------------------------
data Delay = AbsDelay Int -- ^ Absolute Delay
           | RelDelay Dur -- ^ Relative Delay with respect to the current duration

instance Effect Delay where
 effect (AbsDelay n) = return $ eitherSignal (\x -> Mono (replicate n 0.0 ++ x))
                                             (\x -> Stereo (replicate n (0.0:><:0.0) ++ x))

 effect (RelDelay d) = getDur >>= \dur -> let n = absDur (d * dur) in seq n $ return $
                        eitherSignal (\x -> Mono (replicate n 0.0 ++ x))
                                     (\x -> Stereo (replicate n (0.0:><:0.0) ++ x))
-----------------------------------------------------------
data ToMono = ToMono

instance Effect ToMono where
 effect ToMono = return $ Mono . signalToMono
-----------------------------------------------------------
data Average = Average Int

instance Effect Average where
 effect (Average n) = return $ liftSignal (average n)
-----------------------------------------------------------
data Bandpass width slope cutoff = Bandpass width slope cutoff

instance (Sound a, Sound b, Sound c) => Sound (Bandpass a b c) where
 play (Bandpass a b c) = do a' <- playMono a
                            b' <- playMono b
                            c' <- playMono c
                            return $ Mono (bandpass a' b' c')
-----------------------------------------------------------
data SampleAndHold trigger = SampleAndHold trigger

instance Sound a => Effect (SampleAndHold a) where
 effect (SampleAndHold a) = playMono a >>= \a' -> return $ liftSignal (sampleAndHold 0 a')
-----------------------------------------------------------
data Compressor a = Compressor a

instance Sound a => Effect (Compressor a) where
 effect (Compressor a) = playMono a >>= return . liftSignal . zipWith compress
-----------------------------------------------------------
data Morphfilter cutoff = Morphfilter FilterSpec FilterSpec cutoff

instance Sound a => Sound (Morphfilter a) where
 play (Morphfilter fs1 fs2 a) = playMono a >>= \a' ->
                                return $ Mono (morphpass fs1 fs2 a')
-----------------------------------------------------------
data Stretchfilter cutoff = Stretchfilter FilterSpec cutoff

instance Sound a => Sound (Stretchfilter a) where
 play (Stretchfilter fs a) = playMono a >>= \a' ->
                             return $ Mono (stretchpass fs a')

-----------------------------------------------------------
data Follow a = Follow Double a

instance Sound a => Effect (Follow a) where
 effect (Follow d a) = playMono a >>= \a' ->
                           return $ liftSignal (follow d a')
-----------------------------------------------------------
data Panorama balance = Panorama balance

instance Sound a => Effect (Panorama a) where
 effect (Panorama a) = playMono a >>= \a' ->
  return $ eitherSignal (Stereo . zipWith balance a' . map monoToStereo)
                        (Stereo . zipWith balance a')

-----------------------------------------------------------
-- | Width and cutoff.
--data LowpassRect width cutoff = LowpassRect width cutoff

--instance (Sound a, Sound b) => Effect (LowpassRect a b) where
-- effect (LowpassRect a b) = playMono a >>= \a -> playMono b >>= \b -> return $ liftSignal (lowpassrect a b)
-----------------------------------------------------------
