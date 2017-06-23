{-# LANGUAGE FlexibleInstances #-}
module Sound.Hommage.Seq
 ( runNumNotation
 , bpmToDur
 , Seq (..)
 , noteSeq
 , noteSeq'
 , mixdownNumSeq
 , applySeq
-- , applySeqS
 , applySeqE
 , filterSeq
 )
 where

--import Control.Monad.Reader
--import Control.Monad.State
import Data.List
import Data.Ratio

import Sound.Hommage.Misc
import Sound.Hommage.Notation

-------------------------------------------------------------------------------
-- | A 'Seq' represents a temporal sequence of \"Moments\". Each moment
--   can contain some events of type @a@.
newtype Seq a = SEQ { unSeq :: [[a]] }

instance Arrangeable (Seq a) where
 parallel (SEQ xs) (SEQ ys) = SEQ $ merge (++) xs ys
 sequent  (SEQ xs) (SEQ ys) = SEQ (xs ++ ys)

--instance IsDur d => Musical (d -> Seq a) where
-- rest = \d -> SEQ ( replicate (fromIntegral $ absDur $ durFrom d) [] )

instance IsDur d => Musical (WithDur d (Seq a)) where
 rest = WithDur $ \d -> SEQ ( replicate (fromIntegral $ absDur $ durFrom d) [] )

--instance Musical ((Dur, s) -> Seq a) where
-- rest = \d -> SEQ ( replicate (fromIntegral $ absDur $ fst d) [] )

-- | Creates a 'Seq' with the length that the 'Reader' reads.
--   (The length will be in any case at least 1).
noteSeq :: IsDur d => a -> WithDur d (Seq a)
noteSeq a = WithDur $ \d -> let l = fromIntegral $ absDur $ durFrom d in -- if l > 0 then
                            SEQ ( [a] : replicate (l-1) [] )             -- else emptySeq

noteSeq' :: IsDur d => WithDur d a -> WithDur d (Seq a)
noteSeq' ra = WithDur $ \d -> unWithDur (noteSeq $ unWithDur ra d) d
-------------------------------------------------------------------------------
filterSeq :: (a -> Maybe b) -> Seq a -> Seq b
filterSeq check (SEQ xs) = SEQ $ map filt xs
 where
  filt (a:as) = case check a of
                 Nothing -> filt as
                 Just b  -> b : filt as
  filt []     = []

applySeq :: (s -> s) -> Seq (s -> a) -> s -> (Seq a)
applySeq next (SEQ l) = \init -> SEQ (loop init l)
 where
  loop state (x:xs) = let state' = next state
                      in seq state ((map_ ($ state) x : seq state' (loop state' xs)))
--                      in seq state (map_ ($ state) x : seq state' (loop state' xs))
--                      in (map_ ($ state) x : (loop state' xs))
  loop _     _      = []


applySeqE :: (s -> s) -> Seq (Either (s -> a) (s -> s)) -> s -> Seq a
applySeqE next (SEQ l) = \init -> SEQ (loop init l)
 where
  loop state (x:xs) = let (fs, us) = uneitherlist x
                          state'  = foldl (flip ($)) state us
                          state'' = next state'
                      in seq state' ((map ($ state') fs : seq state'' (loop state'' xs)))
  loop _     _      = []

{-
applySeqS :: (s -> s) -> Seq (State s a) -> Reader s (Seq a)
applySeqS next (SEQ l) = Reader $ \init -> SEQ (loop init l)
 where
  loop state (x:xs) = let (a, state') = runState (iter x) state
                          state'' = next state'
                      in a : {- seq state'' -} (loop state'' xs)
  loop _     _      = []
  iter (f:fs) = f >>= \a -> iter fs >>= \r -> return (a : r)
  iter []     = return []
-}
-------------------------------------------------------------------------------
mixdownNumSeq :: Num a => Seq [a] -> [a]
mixdownNumSeq (SEQ s) = loop s
 where
  -- loop :: Num a => [[[a]]] -> [a]
  loop [] = []
  loop (fs:fl) = case map sum $ transpose fs of
                  []  -> 0 : loop fl
                  h:t -> seq h (h : (merge (+) t (loop fl)))

runNumNotation :: (IsDur d, Num a) => Notation (WithDur d [a]) -> d -> [a]
runNumNotation n = \d -> mixdownNumSeq $ unWithDur (runNotationWith noteSeq' n) d

bpmToDur :: Double -> Dur
bpmToDur bpm = round (44100 * (240.0 / bpm)) % 1
-------------------------------------------------------------------------------
