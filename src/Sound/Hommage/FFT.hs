-- | This module implements the Fast Fourier Transformation purely in Haskell.
module Sound.Hommage.FFT
 ( 
 -- * FFT
   fft
 , fft'
 , fftt 

 -- * FFT
 , fftc
 , fftc' 

 , ffttv 

 , fftco 
 , fftco'

 , rect_transform 
 , rect_filter 
 , rect_filter' 

 , i 
 , w 
 , ws 
 , map2 
 , zip2 
 , evens 
 , odds 
 , drop_odds 
 , appendpair
-- , fitlastlength 
 , reorder 
 , reorder_init 
 , reorder_init'
-- , reorder_overlap 
 , butterfly 
 , butterfly'
 , fft_level 
 , fft_merge 
 , fft_init_overlap 
 , fft_level' 
 , fft_merge' 
 , fft_init 
 , fft_overlap_loop 
 , expandComplex 
 , overlap_curve 
 , mix_overlap 

 )
 where
import Data.Complex
import Sound.Hommage.Misc
-------------------------------------------------------------------------------
-- | The complex value 'i' = @(0 :+ 1)@
i :: Complex Double
i = 0.0 :+ 1.0

-- | The n-th root of 1
w :: Int -> Complex Double
w n = exp (-2 * i * pi / fromIntegral n)

-- | the 2n-th root with exponents 0, 1, .. n. False=inverse (exponents are negated)
ws :: Bool -> Int -> [Complex Double]
ws b n = take n $ map (w1^^) ix 
 where
  w1 = exp (-i * pi / fromIntegral n)
  ix | b         = [0, 1 ..]
     | otherwise = [0, (-1) ..]
-------------------------------------------------------------------------------
map2 :: (a -> a -> b) -> [a] -> [b]
map2 f (x:y:r) = f x y : map2 f r
map2 _ _       = []

map2opt :: (a -> a -> b) -> a -> [a] -> [b]
map2opt f a = loop 
 where
  loop (x:y:r) = f x y : loop r
  loop [x]     = [f x a]
  loop _       = []

zip2 :: [a] -> [a] -> [a]
zip2 (x:xs) (y:ys) = x : y : zip2 xs ys
zip2 _ _ = []

evens (x:xs) = x : odds xs
evens []     = []

odds (_:xs) = evens xs
odds []     = []

-- | returns [] if argument has zero or one element.
drop_odds (x:y:r) = x : evens r
drop_odds _       = []

appendpair :: ([a], [a]) -> [a]
appendpair (x,y) = x++y

--fitlastlength :: Int -> a -> [[a]] -> [[a]]
--fitlastlength n a = loop
-- where
--  loop (x:xs) = case loop xs of
--                [] -> [take n (x ++ repeat a)]
--                xs -> x : xs
--  loop []     = []
-------------------------------------------------------------------------------
-- | list is grouped into sublists with length N (must be power of 2) and bitwise reverse order
reorder :: Int -> [[a]] -> [[a]]
reorder 0 = id
reorder n = map2 zip2 . reorder (n-1)

reorder_r2 :: Int -> a -> [[a]] -> [[a]]
reorder_r2 n a = loop n
 where
  loop 0 = id
  loop n = map2opt zip2 (repeat a) . loop (n-1)

reorder_init (x1:x2:x3:x4:xs) = [x1 :+ x2, x3 :+ x4] : reorder_init xs
reorder_init [x1,x2,x3]       = [[x1 :+ x2, x3 :+ 0]]
reorder_init [x1,x2]          = [[x1 :+ x2, 0]]
reorder_init [x1]             = [[x1 :+ 0, 0]]
reorder_init _                = []

reorder_init' :: [a] -> [[a]]
reorder_init' (x:y:r) = [x,y] : reorder_init' r
reorder_init' _       = []

reorder_init_r2' :: a -> [a] -> [[a]]
reorder_init_r2' a = loop
 where
  loop (x:y:r) = [x,y] : loop r
  loop [x]     = [[x,a]]
  loop _       = []
  
--reorder_overlap :: [[a]] -> [a]
--reorder_overlap (xs:ys:r) = (zip2 xs ys) ++ reorder_overlap (ys:r)
--reorder_overlap _       = []
-------------------------------------------------------------------------------
butterfly :: Complex Double ->  Complex Double -> Complex Double -> (Complex Double, Complex Double)
butterfly (wr :+ wi) (xr :+ xi) (yr :+ yi) = (a, b)
 where
  wyr = wr*yr-wi*yi 
  wyi = wr*yi+yr*wi
  a = ( (xr + wyr) * 0.5 :+ (xi + wyi) * 0.5 )
  b = ( (xr - wyr) * 0.5 :+ (xi - wyi) * 0.5 )

butterfly' :: Complex Double ->  Complex Double -> Complex Double -> (Complex Double, Complex Double)
butterfly' (wr :+ wi) (xr :+ xi) (yr :+ yi) = (a, b)
 where
  wyr = wr*yr-wi*yi 
  wyi = wr*yi+yr*wi
  a = ( (xr + wyr) :+ (xi + wyi) )
  b = ( (xr - wyr) :+ (xi - wyi) )
-------------------------------------------------------------------------------
-- | FFT transformation. Input is grouped into overlapping parts of 2^(N+2) reals and mapped to sublists 
--   with 2^(N+1) complex numbers. 
fft :: Int -> [Double] -> [[Complex Double]]
fft n = fft_level (ws True (2^n)) . fft_init_overlap . reorder_r2 (n-1) 0 . reorder_init

fft_level :: [Complex Double] -> [[Complex Double]] -> [[Complex Double]] 
fft_level [_] = id
fft_level ws  = map2 (\x -> appendpair . fft_merge ws x) . fft_level (drop_odds ws)

fft_merge :: [Complex Double] -> [Complex Double] -> [Complex Double] -> ([Complex Double], [Complex Double])
fft_merge (w:ws) (x:xs) (y:ys) = let (xs', ys') = fft_merge ws xs ys
                                     (x', y') = butterfly w x y
                                 in (x' : xs', y' : ys')
fft_merge _ _ _ = ([], [])

fft_init_overlap (x:y:r) = (zipWith (\x y -> [(x + y)*0.5, (x - y)*0.5]) x y) ++ fft_init_overlap (y:r)
fft_init_overlap _       = []

fft_init (x:y:r) = (zipWith (\x y -> [(x + y)*0.5, (x - y)*0.5]) x y) ++ fft_init r
fft_init _       = []
-------------------------------------------------------------------------------
-- | Inverse FFT transformation. Complex input is grouped into parts with length 2^(N+1) and mapped to 
--   sublists with 2^(N+2) reals, which are overlapped and mixed.
fft' :: Int -> [Complex Double] -> [Double]
fft' n = mix_overlap (2^(n+2)) . map expandComplex . fft_level' (ws False (2^n)) . fft_init' . reorder (n-1) . reorder_init'

fft_level' :: [Complex Double] -> [[Complex Double]] -> [[Complex Double]] 
fft_level' [_] = id
fft_level' ws  = map2 (\x -> appendpair . fft_merge' ws x) . fft_level' (drop_odds ws)

fft_merge' :: [Complex Double] -> [Complex Double] -> [Complex Double] -> ([Complex Double], [Complex Double])
fft_merge' (w:ws) (x:xs) (y:ys) = let (xs', ys') = fft_merge' ws xs ys
                                      (x', y') = butterfly' w x y
                                  in (x' : xs', y' : ys')
fft_merge' _ _ _ = ([], [])

fft_init' (x:y:r) = (zipWith (\x y -> [x + y, x - y]) x y) ++ fft_init' r
fft_init' _       = []

fft_overlap_loop (x:r@(y:_)) = (concat $ zipWith (\x y -> [x + y, x - y]) x y) : fft_overlap_loop r
fft_overlap_loop _           = []

expandComplex ((x:+y):r) = x : y : expandComplex r
expandComplex _ = []
-------------------------------------------------------------------------------
-- | Creates a fade with length N
overlap_curve :: Int -> [Double]
overlap_curve n = take n $ map (\k -> 0.5 - 0.5 * cos (fromIntegral k * 2 * pi / fromIntegral n )) [0..]

-- | Overlaps a sequence of parts of length N (overlaps by N\/2).
mix_overlap :: Int -> [[Double]] -> [Double]
mix_overlap n xs = merge (+) l r
  where 
  c = cycle $ overlap_curve n
  l = zipWith (*) c $ concat $ evens xs
  r = replicate (div n 2) 0.0 ++ (zipWith (*) c $ concat $ odds xs)
-------------------------------------------------------------------------------
-- | FFT transformation with overlapping and windowing. 
--   Argument function maps the coefficients. Windowsize: 2^(N+2), i. e. 2^(N+1) coefficients
fftt :: Int -> ([[Complex Double]] -> [Complex Double]) -> [Double] -> [Double]
fftt n f = 
 mix_overlap (2^(n+2)) . 
 map expandComplex . 
 fft_level' (ws False (2^n)) . 
 fft_init' . 
 reorder (n-1) . 
 reorder_init' .
 f .
 fft_level (ws True (2^n)) . 
 fft_init_overlap . 
 reorder_r2 (n-1) 0 . 
 reorder_init

ffttv :: Int -> [Double] -> [Double] -> [Double]
ffttv n vs = fftt n (zipWith (\v (x:+y) -> (v*x) :+ (v*y)) vs . concat . map (\((a:+_):r) -> (a:+0):r))
-------------------------------------------------------------------------------
-- | FFT transformation for complex input (segments of length 2^n). 
--   No overlapping or windowing.
fftc :: Int -> [Complex Double] -> [[Complex Double]]
fftc n = fft_level (ws True (2^n)) . fft_init . reorder_r2 (n-1) 0 . reorder_init_r2' 0

-- | Inverse FFT transformation for complex input (segments of length 2^n). 
--   No overlapping or windowing.
fftc' :: Int -> [Complex Double] -> [[Complex Double]]
fftc' n = fft_level' (ws False (2^n)) . fft_init' . reorder (n-1) . reorder_init'
-------------------------------------------------------------------------------
-- | FFT for complex input with overlapping. Segment-size: 2^(n+1)
fftco :: Int -> [Complex Double] -> [[Complex Double]]
fftco n = fft_level (ws True (2^n)) . fft_init_overlap . reorder_r2 (n-1) 0 . reorder_init_r2' 0

fftco' :: Int -> [Complex Double] -> [Complex Double]
fftco' n = mix_overlapc (2^(n+1)) . fft_level' (ws False (2^n)) . fft_init' . reorder (n-1) . reorder_init'

-- | Overlaps a sequence of parts of length N (overlaps by N\/2).
mix_overlapc :: Int -> [[Complex Double]] -> [Complex Double]
mix_overlapc n xs = merge (+) l r
  where 
  c = cycle $ map (:+0) $ overlap_curve n
  l = zipWith (*) c $ concat $ evens xs
  r = replicate (div n 2) 0.0 ++ (zipWith (*) c $ concat $ odds xs)
-------------------------------------------------------------------------------

-- | A self-inverse transformation similar to FFT but with a simple butterfly
--   operation that uses always W=1. Modifying the data between application
--   and inverse is similar to filtering but the result will be built up from
--   rectangle waves instead of sinus waves.
rect_transform :: Floating a => Int -> [a] -> [[a]]
rect_transform n = run n . map (:[])
 where
  sq = 1 / sqrt 2
  add x y = (x+y) * sq
  sub x y = (x-y) * sq
  run 0 = id
  run n = loop . run (n-1)
  loop (x:y:r) = iter True x y : loop r
  loop [x]     = [iter True x (repeat 0)]
  loop _       = []
  iter True  (x:xs) (y:ys) = add x y : sub x y : iter False xs ys
  iter False (x:xs) (y:ys) = sub x y : add x y : iter True  xs ys
  iter _     _      _      = []

rect_filter :: Floating a => Int -> ([[a]] -> [[a]]) -> [a] -> [a]
rect_filter n f =  concat . rect_transform n . concat . f . rect_transform n

rect_filter' :: Floating a => Int -> [[a]] -> [a] -> [a]
rect_filter' n vs =  concat . rect_transform n . concat . zipWith f vs . rect_transform n
 where
  f v c = zipWith (*) (v ++ repeat 0) c


