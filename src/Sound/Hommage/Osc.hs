module Sound.Hommage.Osc
 (
 -- * Sound Generators
   osc
 , sinus 
 , cosinus 
 , rect
 , saw
 , tri
 , randomList

 -- * Functions for Lists
-- , amplify
 , sampleAndHold
 , average 
 , terminateAt
 , follow

 -- * Functions for single values
 -- | These Functions can be used with 'map':
 , compress
 , noteToFrequency 
 , adjustFrequency 

 -- * Other Functions
 -- | These Functions are not simple (i. e. linear) list transformers:
 , splitWaves 
 , crossfade 
 )
 where

import Sound.Hommage.Misc
import System.Random

{- Length Stepsize Freq (Hz) -}

---------------------------------------------------------------------------------------------------
randomList :: Random a => (a,a) -> [a]
randomList = toList . return . randomRIO 
---------------------------------------------------------------------------------------------------
-- | Play given sound with variable speed resp. frequency. 
--   (General definition: Usually a frequency of 1.0 means a period of 1024 values).
--   Use 'scratchSample' or 'scratchSampleSignal' for backward playing.
osc :: [Double] -- ^ The sound to play. Output will be finite if sound is finite.
    -> [Double] -- ^ Speed,      1.0 \= normal, 
                --               0.0 \< X \< 1.0 \= slower resp. lower, 
                --               1.0 \< X \= faster resp. higher, 
                --               X \< 0.0 \=\> X \= abs X.
    -> [Double] -- ^ Output 
osc = loop 0.0
 where
  loop p []  _     = []
  loop p vs (d:ds) = let v   = interpol p vs
                         p'  = p + abs d
                         i   = floor p'
                     in v : loop (p' - fromIntegral i) (drop (floor p') vs) ds 
  loop p _      _  = []
  interpol p ds = if p == 0.0 then head ds else
                  case ds of
                   (d1 : d2 : r) -> d1 * (1.0 - p) + d2 * p
                   [d1]          -> d1 * (1.0 - p) 
---------------------------------------------------------------------------------------------------
integrate :: Num a => [a] -> [a]
integrate [] = []
integrate (x:xs) = loop x xs
 where
  loop v (a:as) = let k = v + a in seq k (v : loop k as)
--  loop v (a:as) = let k = v + a in v : seq k (loop k as) -- ODER SO?
  loop v []     = [v]

-- | A sinus wave generator with a period of 1024\/N for frequency N 
sinus :: [Double] -> [Double]
sinus = map (sin . (*f)) . integrate  
 where
  f = pi / 512.0

-- | A cosinus wave generator
cosinus :: [Double] -> [Double]
cosinus = map (cos . (*f)) . integrate 
 where
  f = pi / 512.0

-- | A rectangle wave generator
rect :: [Double] -> [Double]
rect = map signum . sinus

-- | A sawtooth wave generator
saw :: [Double] -> [Double]
saw = loop 1.0
 where
  loop _ []     = []
  loop v (d:dr) | v <= -1.0 = loop 1.0 (d:dr)
                | otherwise = v : loop (v - abs (d / 512.0)) dr

-- | A triangle wave generator
tri :: [Double] -> [Double]
tri = osc (let k = 0.0 : 1.0 : 0.0 : -1.0 : k in k) . map (/256.0)
---------------------------------------------------------------------------------------------------
-- | Adjusts the frequency. If given oscillator has period X for frequency of 1.0 and you want it to
--   produce a wave with Y Hz at frequency of 1.0, use @ map (adjustFrequency X Y) @ to adjust the
--   input of the oscillator.
adjustFrequency :: Double   -- ^ Period 
                -> Double   -- ^ New Frequency (Hz) for old frequency of 1.0
                -> Double   -- ^ Input Frequency
                -> Double   -- ^ Output Frequency
adjustFrequency periode basefreq = let k = periode * basefreq / 44100.0 in (*k)

-- | Transforms a notevalue into a frequency. A Notevalue of 0.0 means a frequency of 1.0. 
noteToFrequency :: Double  -- ^ Base, 
                -> Double  -- ^ Notenumber
                -> Double  -- ^ 2 \^ (Notenumber \/ Base)
noteToFrequency base note = 2.0 ** (note / base)
---------------------------------------------------------------------------------------------------
-- | Splits a wave into parts. they are split when a value equal or less than zero is followed by a
--   value greater than zero.
splitWaves :: [Double] -> [[Double]]
splitWaves = loop
 where
  next l (x:xs) | l <= 0.0 && x > 0.0 = ([], x:xs)
                | otherwise           = let (a,r) = next x xs in (x:a,r)
  next l []                           = ([],[])
  loop (x:xs) = let (a,r) = next x xs in ((x:a) : loop r)
  loop []     = []

---------------------------------------------------------------------------------------------------
-- | Create a wave with the beginning of w1, the ending of w2 and the length of the longer one of them.
crossfade :: [Double] -- ^ w1
          -> [Double] -- ^ w2
          -> [Double] -- ^ result
crossfade xs1 xs2 = fun
 where
  l1 = length xs1
  l2 = length xs2
  fun | l1 < l2 = let (i,r) = splitAt l1 xs2
                      d = 1.0 / fromIntegral l1
                  in (zipWith3 (\v x y -> (1.0-v)*x + v*y) (iterate (+d) 0.0) xs1 i) ++ r
      | l1 > l2 = let (i,r) = splitAt (l1-l2) xs1
                      d = 1.0 / fromIntegral l2
                  in i ++ (zipWith3 (\v x y -> (1.0-v)*x + v*y) (iterate (+d) 0.0) r xs2)
      | otherwise = let d = 1.0 / fromIntegral l1
                    in (zipWith3 (\v x y -> (1.0-v)*x + v*y) (iterate (+d) 0.0) xs1 xs2)
---------------------------------------------------------------------------------------------------
-- | @ compress p x = x \/ (abs p + abs x) @ 
compress :: Double -> Double -> Double
compress 0.0 x = signum x
compress p x   = x / (abs p + abs x)

---------------------------------------------------------------------------------------------------
-- | Current output value is repeatet until the first list argument value switches from zero or below 
--   to a non-zero positive value, the actual value of the second list argument is then taken for output.
sampleAndHold :: (Ord a, Num a) => b -> [a] -> [b] -> [b]
sampleAndHold y xs ys = loop y 0 xs ys
 where
  loop s o (a:as) (b:bs) | o <= 0 && a > 0 = b : loop b a as bs
                         | otherwise       = s : loop s a as bs
  loop _ _ _ _ = []

follow :: Double -> [Double] -> [Double] -> [Double] 
follow p = loop 0.0 
 where
  loop pos (f:fs) (x:xs) | pos > x   = pos : loop (pos - p * abs f) fs xs
                         | otherwise = pos : loop (pos + p * abs f) fs xs
  loop _ _ _ = []

-- | Maps the values to the average of the last N values (including the actual)
average :: Fractional a => Int -> [a] -> [a]
average n as = loop 0 as (replicate n 0 ++ as)
 where
  dy = 1.0 / fromIntegral n
  loop i (x:xs) (y:ys) = (i * dy) : loop (i+x-y) xs ys
  loop _ _ _ = []

---------------------------------------------------------------------------------------------------
varydelay :: Int -> [Int] -> [a] -> [a]
varydelay len (l:ls) (d:ds) | len > l   = d : varydelay (len - 1) ls (tail ds)
                            | len < l   = d : d : varydelay (len + 1) ls ds
                            | otherwise = d : varydelay len ls ds
varydelay _   _      _      = []


variableDelay :: Int -> [Double] -> [Double] -> [Double]
variableDelay initLen pitch inp = outp
 where
  len0       = fromIntegral initLen 
  lengths    = map (floor.(len0/). max 0.001 . abs) pitch
  outp       = varydelay initLen lengths (replicate initLen 0.0 ++ inp)
---------------------------------------------------------------------------------------------------
-- | If predicate holds for N elements, list is cut.
terminateAt :: Int -> (a -> Bool) -> [a] -> [a]
terminateAt n test = loop n
 where
  loop 0 (x:xs) = if test x then [] else x : loop n xs
  loop k (x:xs) = if test x then x : loop (k-1) xs else x : loop n xs
               
---------------------------------------------------------------------------------------------------
