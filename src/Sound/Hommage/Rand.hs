module Sound.Hommage.Rand
 ( R (..)
 , mkR
 , runR
 , rif
 , rselect
 , RM
 , ror
 , rnote
 , runRM
 , rplay
 )
 where

import Control.Applicative
import System.Random

import Sound.Hommage.Misc
import Sound.Hommage.Notation
import Sound.Hommage.Play
import Sound.Hommage.Sound
import Sound.Hommage.Signal

---------------------------------------------------------------------------------------------------
newtype R a = R { unR :: StdGen -> a }

instance Monad R where
 return = R . const
 R g >>= f = R $ \s -> let (s1,s2) = split s in unR (f (g s1)) s2

--instance Applicative R where
--    R g <*> R h = R $ \s ->
--      let (s1,s2) = split s
--      in g s (h s)

instance Functor R where
 fmap f (R g) = R (f . g)

mkR :: (StdGen -> (a, StdGen)) -> R a
mkR f = R (fst . f)


instance Applicative R where
 R gf <*> R ga = R $ \s -> let (s1,s2) = split s in gf s1 (ga s2)
 pure = R . const

runR :: Int -> R a -> a
runR i (R g) = g $ mkStdGen i

rif :: Double -> (a , a) -> R a
rif p (a1,a2) = R $ \s -> if (fst $ randomR (0, 1::Double) s) > p then a1 else a2

rselect :: [a] -> R a
rselect [] = error "empty probs"
rselect xs = let n = fromIntegral $ length xs
             in R $ \s -> let k = fst $ randomR (0, 1::Double) s
                              i = floor (k * n)
                          in xs !! i

---------------------------------------------------------------------------------------------------
newtype RM a = RM { unRM :: StdGen -> Notation a }

instance Functor RM where
 fmap f (RM g) = RM ( fmap f . g)

instance Applicative RM where
    pure a = RM $ const $ note a
    RM g <*> RM h = RM $ \s ->
        let (s1,s2) = split s
        in g s Prelude.<*> h s

instance Monad RM where
 return a = RM $ const $ note a
 RM g >>= f = RM $ \s ->
    let (s1,s2) = split s
    in g s1 >>= \a-> unRM (f a) s2

--rRM :: R a -> (a -> RM b) -> RM b
--rRM (R g) f = RM $ \s -> let (s1,s2) = split s in unRM (f (g s1)) s2

instance Stretchable (RM a) where
 stretch d (RM g) = RM (stretch d . g)

instance Arrangeable (RM a) where
 parallel (RM a) (RM b) = RM $ \s -> let (s1,s2) = split s in a s1 :=: b s2
 sequent  (RM a) (RM b) = RM $ \s -> let (s1,s2) = split s in a s1 :+: b s2

instance Musical (RM a) where
 rest = RM $ const $ Rest 1

rnote :: a -> RM a
rnote = RM . const . Note 1

ror :: [RM a] -> RM a
ror [] = rest
ror xs = let n = fromIntegral $ length xs
         in RM $ \s -> let (s1, s2) = split s
                           k = fst $ randomR (0, 1::Double) s1
                           i = floor (k * n)
                           m = xs !! i
                       in unRM m s2

runRM :: RM a -> R (Notation a)
runRM (RM g) = R g

rplay :: Sound a => a -> RM (Play Signal)
rplay = rnote . play

