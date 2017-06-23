{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains some functions and datatypes for envelopes.
--   An envelope in generall is a kind of controll signal that modulates
--   (for example) the volume of a sound in a non-periodic way (getting
--   loud at the beginning and fading out at the end...). It is
--   also used to controll the duration of a sound, since (here) envelopes
--   always produce finite signals.
module Sound.Hommage.Envelope
 (

 -- | An envelope is represented by a function of type 'Env'.
 --   Such functions (or instances of class 'IsEnv') can be combined
 --   into a sequence where every 'Env' is given a 'EnvLength' (using the
 --   binary operator '<?>'.
 --   The whole sequence can be turnde into an 'Env' (and be played then)
 --   with 'runEnv'.

 -- * Env
   Env
 , runEnv
 , (<?>)
 , IsEnv (..)
 -- * EnvLength
 , calculateEnvLengths
 , EnvLength (..)

 -- * ADSR
 , ADSR
 , EnvShape (..)
 , EnvMode (..)
 , playADSR

 -- | Here is the code of 'playADSR' to show the use of 'Env', '<?>' and 'EnvLength':
 --
 -- > playADSR :: EnvMode -> EnvShape -> ADSR -> Int -> [Double]
 -- > playADSR mode shape (a,d,s,r) = case mode of
 -- >  FitADR -> runEnv
 -- >            [ Interpolate shape (0.0,1.0) <?> Flex (fromIntegral a)
 -- >            , Interpolate shape (1.0, s) <?> Flex (fromIntegral d)
 -- >            , Interpolate shape (s, 0.0) <?> Flex (fromIntegral r)
 -- >            ]
 -- >  FitS   -> runEnv
 -- >            [ Interpolate shape (0.0,1.0) <?> Abs a
 -- >            , Interpolate shape (1.0, s) <?> Abs d
 -- >            , Constant s <?> Flex 1.0
 -- >            , Interpolate shape (s, 0.0) <?> Abs r
 -- >            ]
 -- >  HoldS  -> runEnv
 -- >            [ Interpolate shape (0.0,1.0) <?> Abs a
 -- >            , Interpolate shape (1.0, s) <?> Abs d
 -- >            , Constant s <?> Flex 1.0
 -- >            , Interpolate shape (s, 0.0) <?> Abs_ r
 -- >            ]

 -- * Interpolate
 , Interpolate (..)
 , Constant (..)
 , interpolate
 , interpolate_cos


 )
 where

import Data.Ratio

---------------------------------------------------------------------------------------------------
-- | 'EnvLength' represents the length of a segment of an Envelope.
data EnvLength = Abs Int            -- ^ A fixed length.
               | Abs_ Int           -- ^ A fixed length that is not subtracted from the total time.
               | Flex Double        -- ^ A flexible length. Resuming length is distributed to all
                                    --   flexible lengths proportionally to its value.
               | Rel (Ratio Int)    -- ^ A length relative to the total length.
               | Rel_ (Ratio Int)   -- ^ A relative length that is not subtracted from the total time.

-- | Takes an absolute total length and a list of EnvLengths. Each 'EnvLength' is mapped
--   to its length with respect to the total length and a resuming length that is
--   the result of @ total length - (all fixed lengths + all relative lengths) @.
--   This resuming lengths is distributed to the flexible lengths.
calculateEnvLengths :: Int -> [EnvLength] -> [Int]
calculateEnvLengths n eds = map lengthE normed_eds
 where
  fixlen = sum ( eds >>= \ed -> case ed of { Abs i -> [i]; _ -> [] } )
  rellen = round (fromIntegral n * sum (eds >>= \ed -> case ed of { Rel r -> [r]; _ -> [] } ))
  len = max 0 (n - (fixlen + rellen))
  flexsum = sum (eds >>= \ed -> case ed of { Flex d -> [abs d]; _ -> [] })
  normed_eds = if flexsum == 0.0 then eds else
               map (\e -> case e of { Flex d -> Flex (d / flexsum); a -> a }) eds
  lengthE :: EnvLength -> Int
  lengthE (Abs i)     = i
  lengthE (Abs_ i)    = i
  lengthE (Flex d)  = round (d * fromIntegral len)
  lengthE (Rel r)  = round (r * fromIntegral n)
  lengthE (Rel_ r) = round (r * fromIntegral n)
---------------------------------------------------------------------------------------------------
type Env = Int -> [Double]

runEnv :: [(Env, EnvLength)] -> Env
runEnv eps n = concat $ zipWith ($) (map fst eps) $ calculateEnvLengths n $ map snd eps
---------------------------------------------------------------------------------------------------
class IsEnv a where
 toEnv :: a -> Env

instance IsEnv Env where
 toEnv = id

(<?>) :: IsEnv a => a -> EnvLength -> (Env, EnvLength)
a <?> l = (toEnv a, l)
---------------------------------------------------------------------------------------------------
-- | A linear or a cosinus-like shape
data EnvShape = Linear | CosLike
 deriving (Eq, Read, Show)

data Interpolate = Interpolate EnvShape (Double, Double)

instance IsEnv Interpolate where
 toEnv (Interpolate Linear v) = interpolate v
 toEnv (Interpolate CosLike v) = interpolate_cos v

-- | produces a line with given length that starts with fst value and ends with snd value
interpolate :: Fractional a => (a, a) -> Int -> [a]
interpolate (i,e) len =
 let d = (e - i) / fromIntegral len
 in take len (iterate (+d) i)

-- | produces a curve with given length that starts with fst value and ends with snd value.
--   this curve has the shape of a half cosinus curve (values for 0 to PI).
interpolate_cos :: Floating a => (a, a) -> Int -> [a]
interpolate_cos (i,e) len = map fun [0 .. len-1]
 where
     dlen = fromIntegral len
     diff = e - i
     d   = diff * 0.5
     fak = pi / dlen
     fun x = i + d * (1.0 - cos (fak * fromIntegral x))
---------------------------------------------------------------------------------------------------
data Constant = Constant Double

instance IsEnv Constant where
 toEnv (Constant d) = flip replicate d
---------------------------------------------------------------------------------------------------
-- | The four components of 'ADSR' are:
--
--   * Attack (time to reach value 1.0, starting from 0.0)
--
--   * Decay (time to reach sustain level)
--
--   * Sustain (level to hold until note is released, should be a value between 0.0 and 1.0)
--
--   * Release (time to reach value 0.0 after note is released)
--
-- time is measured in sample points, 44100 is one second.
type ADSR = (Int,Int,Double,Int)
---------------------------------------------------------------------------------------------------
data EnvMode = HoldS   -- ^ Sustain value is kept until duration is over, Release part is appended.
             | FitS    -- ^ Envelope has given duration by fitting only duration of constant Sustain level.
             | FitADR  -- ^ Attack, Decay and Released are together stretched to given duration.
 deriving (Eq, Read, Show)

-- | Playing an ADSR
playADSR :: EnvMode -> EnvShape -> ADSR -> Int -> [Double]
playADSR mode shape (a,d,s,r) = case mode of
 FitADR -> runEnv
           [ Interpolate shape (0.0,1.0) <?> Flex (fromIntegral a)
           , Interpolate shape (1.0, s) <?> Flex (fromIntegral d)
           , Interpolate shape (s, 0.0) <?> Flex (fromIntegral r)
           ]
 FitS   -> runEnv
           [ Interpolate shape (0.0,1.0) <?> Abs a
           , Interpolate shape (1.0, s) <?> Abs d
           , Constant s <?> Flex 1.0
           , Interpolate shape (s, 0.0) <?> Abs r
           ]
 HoldS  -> runEnv
           [ Interpolate shape (0.0,1.0) <?> Abs a
           , Interpolate shape (1.0, s) <?> Abs d
           , Constant s <?> Flex 1.0
           , Interpolate shape (s, 0.0) <?> Abs_ r
           ]

