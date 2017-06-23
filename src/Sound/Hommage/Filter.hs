module Sound.Hommage.Filter
 (
 -- * Filters
   lowpass
 , highpass
 , bandpass
 , stretchpass
 , morphpass 
-- , vocoder

-- , lowpassrect 

 -- * FilterSpec
-- , specfilter 
 , FilterSpec  
 , shiftFilterSpec 

 -- * FilterSpec and Lists
 , filterSpecToList
 , filterSpecsToLists
 , interpolFilterLists 

 -- * Other
 , ffreq 
 , ffreqi
 , average512 

 -- * Filterfunctions
 , lowpassFS
 , highpassFS
 , bandpassFS 
 , lowp0FS 
 , lowp1FS
 , bandp0FS 
 , bandp1FS 
 , highp0FS 
 , highp1FS 

 )
 where

--import Sound.Hommage.DFTFilter
import Data.Complex

--import Sound.Hommage.FFT
---------------------------------------------------------------------------------------------------
lowpass :: [Double] -> [Double] -> [Double]
lowpass s c = concat $ map filterSpecToList $ 
 zipWith lowpassFS (average512 s) (average512 c)

highpass :: [Double] -> [Double] -> [Double]
highpass s c = concat $ map filterSpecToList $ 
 zipWith highpassFS (average512 s) (average512 c)

bandpass :: [Double] -> [Double] -> [Double] -> [Double] 
bandpass r s c = concat $ map filterSpecToList $ 
 zipWith3 bandpassFS (average512 r) (average512 s) (average512 c)

stretchpass:: FilterSpec -> [Double] -> [Double] 
stretchpass fs c = concat $ map filterSpecToList $ 
 map (flip shiftFilterSpec fs) $ map (2.0**) $ average512 c
 
morphpass:: FilterSpec -> FilterSpec -> [Double] -> [Double]
morphpass f1 f2 c = 
 let l1 = filterSpecToList f1
     l2 = filterSpecToList f2
 in concat $ map (interpolFilterLists l1 l2) $ average512 c
---------------------------------------------------------------------------------------------------
{-




-- | first argument is width of filter, second is cutoff
lowpassfilter :: [Double] -> [Double] -> [Double] -> [Double]
lowpassfilter s c = 

lowpassrect :: [Double] -> [Double] -> [Double] -> [Double]
lowpassrect s c =  
 let i = ([]:) $ map filterSpecToList $ zipWith lowpassFS (average512 s) (average512 c)
 in rect_filter' 9 i

{-
specfilterFFT :: [FilterSpec] -> [Double] -> [Double]
specfilterFFT fs = fftt 8 (concat . zipWith fun fs )

fun :: FilterSpec -> [Complex Double] -> [Complex Double] 
fun fs ((c0 :+ _) : cs) = zipWith f ((c0 :+ 0) : cs) (filterSpecToList fs)
 where
  f (x:+y) v = (x * v) :+ (y * v)
-}

-- | first argument is width of filter, second is cutoff
highpassfilter :: [Double] -> [Double] -> [Double] -> [Double]
highpassfilter s c = 
 let i = concat $ map filterSpecToList $ zipWith highpassFS (average512 s) (average512 c)
 in dftfilter i

-- | first argument is width of filter, second is slope, third is cutoff
bandpassfilter :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
bandpassfilter r s c = 
 let i = concat $ map filterSpecToList $ zipWith3 bandpassFS (average512 r) (average512 s) (average512 c)
 in dftfilter i

stretchfilter :: FilterSpec -> [Double] -> [Double] -> [Double]
stretchfilter fs c = 
 let i = concat $ map filterSpecToList $ map (flip shiftFilterSpec fs) $ map (2.0**) $ average512 c
 in dftfilter i

morphfilter :: FilterSpec -> FilterSpec -> [Double] -> [Double] -> [Double]
morphfilter f1 f2 c = 
 let l1 = filterSpecToList f1
     l2 = filterSpecToList f2
     i = concat $ map (interpolFilterLists l1 l2) $ average512 c
 in dftfilter i

---------------------------------------------------------------------------------------------------
specfilter :: [FilterSpec] -> [Double] -> [Double]
specfilter fs = 
 let i = concat $ map filterSpecToList fs
 in dftfilter i
-}
---------------------------------------------------------------------------------------------------
-- | for filter coefficient number: (0.0 .. 1.0) -> (1.0 .. 512.0)
ffreq :: Double -> Double
ffreq k = r
 where
  k' = abs k 
  r | k' > 1.0  = 512.0
    | otherwise = 2 ** (9.0 * k')

-- | floor of 'ffreq' 
ffreqi :: Double -> Int
ffreqi = floor . ffreq 

-- | the resulting list is 1 \/ 512 as long as the input list. 
--   512 elements are read and their average is the next output value.
average512 :: [Double] -> [Double]
average512 = loop 512 0.0
 where
  loop 0 s xs     = (s / 512.0) : loop 512 0.0 xs
  loop k s (x:xs) = loop (k-1) (s+x) xs
  loop _ _ _      = []
--mkFilterFun :: (Double -> FilterSpec) -> (Double, Double) -> FilterFun Double
--mkFilterFun filt (cutoff, fmod) = withFilterEnv . mkFilterEnv . filt . (+cutoff) . (*fmod)
---------------------------------------------------------------------------------------------------
-- | Filtering starts at coeff nr 0 and value 0.0 (constant coeff is always zero). 
--   The elements in FilterSpec define the next value and how many 
--   coeffs it takes to reach this value. 
type FilterSpec   = [(Int, Double)] 

-- | 0..1: lower, 1..: higher
shiftFilterSpec :: Double -> FilterSpec -> FilterSpec 
shiftFilterSpec d fs = map f fs
 where
  f (n,v) = (round (d * fromIntegral n), v)

-- | Converting a FilterSpec to a list of 512 Doubles
filterSpecToList :: FilterSpec -> [Double]
filterSpecToList xs = take 512 $ loop 0.0 xs
 where
  loop c []          = repeat c
  loop c ((n,d): ls) = let f = (d-c) / fromIntegral n in take n (iterate (+f) c) ++ loop d ls


-- | Converting and interpolating a sequence of FilterSpecs to a list of lists. The first elemet of the
-- tuple, an Int, describes how many lists it takes to reach the given FilterSpec. 
filterSpecsToLists :: [(Int, FilterSpec)] -> [[Double]]
filterSpecsToLists = loop (filterSpecToList  [])
 where
  loop l []         = []
  loop l ((n,f):xs) = let f' = filterSpecToList f 
                      in fun n l f' ++ loop f' xs
  fun n f1 f2       = let d = 1.0 / fromIntegral n
                      in take n $ map (interpolFilterLists f1 f2) $ iterate (+d) 0.0

interpolFilterLists :: [Double] -> [Double] -> Double -> [Double]
interpolFilterLists f1 f2 n | n >= 1.0  = f2
                            | n <= 0.0  = f1
                            | otherwise = let n' = 1.0 - n in 
                                          zipWith (\a b -> a * n' + b * n) f1 f2

---------------------------------------------------------------------------------------------------
lowpassFS :: Double -> Double -> FilterSpec
lowpassFS d c = [(1,1.0), (round fd, 1.0), (round (fd * abs d), 0.0)]
 where
  fd = ffreq c

highpassFS :: Double -> Double -> FilterSpec
highpassFS d c = [(round fd, 0.0), (round (fd * abs d), 1.0)]
 where
  fd = ffreq c

-- | range, curve, cutoff
bandpassFS :: Double -- ^ width of freq-window
           -> Double -- 
           -> Double -- ^ cutoff
           -> FilterSpec
bandpassFS r s f = [(l1, 0.0), (l2, 1.0), (l3, 1.0), (l4, 0.0)]
 where

  m1 = abs r + 1
  m2 = m1 + abs s
  a = ffreqi (f / m2)
  b = ffreqi (f / m1)
  c = ffreqi (f * m1)
  d = ffreqi (f * m2)
  
  l1 = a
  l2 = b - a
  l3 = c - b
  l4 = d - c

bandpassFS' :: Double -- ^ width of freq-window
           -> Double -- 
           -> Double -- ^ cutoff
           -> FilterSpec
bandpassFS' r s f = [(1 + round a, 0.0), (1 + round b, 1.0), (1 + round c, 1.0), (1 + round d, 0.0)]
 where
  r' = 1.0 + abs r
  s' = 1.0 + abs s
  fd = ffreq f
  a  = fd
  b  = fd * s' - a
  c  = b * r'  - b
  d  = c * s'  - c
---------------------------------------------------------------------------------------------------
lowp0FS :: Double -> FilterSpec
lowp0FS f = [(0, 1.0), (fr, 1.0), (1, 0.0)]
 where
  fr = ffreqi f

lowp1FS :: Double -> FilterSpec
lowp1FS f = [(0, 1.0), (fr, 1.0), (fr, 0.0)]
 where
  fr = ffreqi f

bandp0FS :: Double -> FilterSpec
bandp0FS f = [(f0-1, 0.0), (f0, 1.0), (fr, 1.0), (1, 0.0)]
 where
  fr = ffreqi f
  f0 = fr - (div fr 2) + 1

bandp1FS :: Double -> FilterSpec
bandp1FS f = [(fh, 0.0), (fh, 1.0), (fh, 0.0)]
 where
  fr = ffreqi f
  fh = div fr 2

highp0FS :: Double -> FilterSpec
highp0FS f = [(fr, 0.0), (1, 1.0)]
 where
  fr = ffreqi f

highp1FS :: Double -> FilterSpec
highp1FS f = [(fh, 0.0), (fh, 1.0)]
 where
  fr = ffreqi f
  fh = div fr 2
---------------------------------------------------------------------------------------------------
{-
-- | first argument is control signal
vocoder :: [Double] -> [Double] -> [Double]
vocoder xs ys = dftsynthese $ zipWith f (dftanalyse xs) (dftanalyse ys)
 where
  f x (a :+ b) = let z = magnitude x in (a*z :+ b*z)
-}
---------------------------------------------------------------------------------------------------
