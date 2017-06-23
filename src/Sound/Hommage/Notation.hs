{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Sound.Hommage.Notation
 (
 -- * Duration
   Dur
 , absDur
 , IsDur (..)
 , WithDur (..)
 -- * Music Notation
 , Notation (..)
 , runNotation
 , runNotationWith
 -- * Musical class
 , Stretchable (..)
 , Arrangeable (..)
 , Musical (..)
 , rest0
 , (-=-)
 , (->-)
 , line
 , line'
 , chord
 , proportional

 -- * Notation and Midi
 , writeMidiSyncNotation
 , midi
 , midi'
 , midiSyncFile

 -- * More Notation functions
 , note
 , mapNotation
 , joinNotation
 , unmaybeNotation
 , durationNotation
 , positionNotation
 , reverseNotation
 , takeNotation
 , dropNotation
-- , chordNotation
-- , lineNotation
-- , lineNotation'
-- , propNotation
 , filterNotation
 , filterNotation'
 , sequenceNotation
 )
 where

import Sound.Hommage.Midi

import Data.Ratio
-------------------------------------------------------------------------------
-- | The duration (of a note, e. g).
type Dur = Ratio Int

-- | Calculates the absolute duration by dividing the numerator with the denominator.
--   Because of rounding error this makes only sense if the result is a relative big
--   number.
absDur :: Dur -> Int
absDur d = div (numerator d) (denominator d)
-------------------------------------------------------------------------------
newtype WithDur d a = WithDur { unWithDur :: d -> a }
--
instance IsDur d => Stretchable (WithDur d a) where
 stretch dur (WithDur f) = WithDur $ \d -> f (durUpdate (*dur) d)

instance (IsDur d, Arrangeable a) => Arrangeable (WithDur d a) where
 parallel (WithDur r1) (WithDur r2) = WithDur $ \d ->
                                      parallel (r1 d) (r2 d)
 sequent (WithDur r1) (WithDur r2) = WithDur $ \d ->
                                      sequent (r1 d) (r2 d)
-------------------------------------------------------------------------------
instance Stretchable a => Stretchable (s -> a) where
 stretch d f = stretch d . f

instance Arrangeable a => Arrangeable (s -> a) where
 parallel f g = \s -> parallel (f s) (g s)
 sequent f g = \s -> sequent (f s) (g s)

instance Musical a => Musical (s -> a) where
 rest = const rest
-------------------------------------------------------------------------------
class IsDur d where
 durFrom :: d -> Dur
 durUpdate :: (Dur -> Dur) -> d -> d

instance IsDur Dur where
 durFrom = id
 durUpdate f = f
-------------------------------------------------------------------------------
-- | A type\/structure that can be stretched.
class Stretchable a where
 stretch :: Dur -> a -> a
-------------------------------------------------------------------------------
-- | Types\/structures that can be composed in two ways, parallel and sequent.
class Arrangeable a where
 parallel :: a -> a -> a
 sequent  :: a -> a -> a

(-=-) :: Arrangeable a => a -> a -> a
(-=-) = parallel

(->-) :: Arrangeable a => a -> a -> a
(->-) = sequent

{-
instance Functor ((->)s) where
 fmap f g = f . g
-}
-------------------------------------------------------------------------------
-- | Instances of class 'Musical' must be 'Stretchable', 'Arrangeable' and
--   they must implement the method 'rest'.
class (Stretchable a, Arrangeable a) => Musical a where
 rest :: a

rest0 :: Musical a => a
rest0 = stretch 0 rest

-- | A sequence of sounds
line :: Musical a => [a] -> a
line = foldr (->-) rest0

-- | A sequence of sounds that will be stretched to length=1
line' :: Musical a => [a] -> a
line' [] = rest
line' xs = stretch (1 % length xs) $ foldr1 (->-) xs

chord :: Musical a => [a] -> a
chord = foldr (-=-) rest0

-- | Composes the notations sequentially and stretches them proportionally.
proportional :: Musical a => (Int, Int) -> a -> a -> a
proportional (l,r) c1 c2 | l == r    = stretch (1 % 2) (c1 ->- c2)
                         | otherwise = let l' = abs l
                                           r' = abs r
                                       in stretch (l' % (l'+r')) c1
                                      ->- stretch (r' % (l'+r')) c2
-------------------------------------------------------------------------------
-- | A 'Notation' is a constant, tree-like structure that represents a musical
--   notation. It has a type parameter for flexible usage reasons.
data Notation a = Note Dur a                  -- ^ A note with given duration and a value of type @a@.
                | Rest Dur                    -- ^ A rest with given duration.
                | Notation a :+: Notation a   -- ^ Sequential composition of two notations.
                | Notation a :=: Notation a   -- ^ Parallel composition of two notations.
                | Stretch Dur (Notation a)    -- ^ Stretches the duration of the sub-music by given factor.

-- | Creates a note with length 1. Is a synonym for @Note (1%1)@
note :: a -> Notation a
note a = Note (1%1) a

instance Functor Notation where
 fmap = mapNotation

instance Stretchable (Notation a) where
 stretch = Stretch

instance Arrangeable (Notation a) where
 sequent  = (:+:)
 parallel = (:=:)

instance Musical (Notation a) where
 rest = Rest (1%1)

instance Applicative Notation where
  pure a = Note (1%1) a
  nf <*> na = joinNotation $ fmap (\f -> fmap f na) nf

instance Monad Notation where
 return a = Note (1%1) a
 na >>= f = joinNotation $ fmap f na
-------------------------------------------------------------------------------
-- | A 'Notation' can be interpreted if the contained type is an
--   instance of class 'Musical'.
runNotation :: Musical m => Notation m -> m
runNotation = loop
 where
  loop m = case m of
   Note d a     -> stretch d a
   Rest d       -> stretch d rest
   m1 :+: m2    -> loop m1 `sequent` loop m2
   m1 :=: m2    -> loop m1 `parallel` loop m2
   Stretch d m1 -> stretch d $ loop m1

runNotationWith :: Musical m => (a -> m) -> Notation a -> m
runNotationWith note = loop
 where
  loop m = case m of
   Note d a     -> stretch d $ note a
   Rest d       -> stretch d rest
   m1 :+: m2    -> loop m1 `sequent` loop m2
   m1 :=: m2    -> loop m1 `parallel` loop m2
   Stretch d m1 -> stretch d $ loop m1
-------------------------------------------------------------------------------
-- | 'Notation' is instance of the class Functor.
mapNotation :: (a -> b) -> Notation a -> Notation b
mapNotation f m = case m of
 Note d a     -> Note d (f a)
 Rest d       -> Rest d
 m1 :+: m2    -> mapNotation f m1 :+: mapNotation f m2
 m1 :=: m2    -> mapNotation f m1 :=: mapNotation f m2
 Stretch d m1 -> Stretch d (mapNotation f m1)

-- | 'Notation' is instance of the class Monad. Joining will replace
--    every (outer) Note by its contained (inner) Notation. The inner
--    Notation will be stretched by the duration of the (outer) Note.
joinNotation :: Notation (Notation a) -> Notation a
joinNotation m = case m of
 Note d a     -> Stretch d a
 Rest d       -> Rest d
 m1 :+: m2    -> joinNotation m1 :+: joinNotation m2
 m1 :=: m2    -> joinNotation m1 :=: joinNotation m2
 Stretch d m1 -> Stretch d (joinNotation m1)


-- | Replaces any Note that contains Nothing by a rest (with same duration).
unmaybeNotation :: Notation (Maybe a) -> Notation a
unmaybeNotation = loop
 where
  loop m = case m of
   Note d (Just a) -> Note d a
   Note d _        -> Rest d
   Rest d          -> Rest d
   m1 :+: m2       -> loop m1 :+: loop m2
   m1 :=: m2       -> loop m1 :=: loop m2
   Stretch d m1    -> Stretch d $ loop m1
-------------------------------------------------------------------------------
-- | A @ Notation MidiNote @ can be interpreted using 'runNotationWith' and 'midi'.
midi :: IsDur d => MidiNote -> WithDur d MidiMusic
midi n = WithDur $ \d -> noteMidiMusic (fromIntegral $ absDur $ durFrom d) n

midi' :: IsDur d => WithDur d MidiNote -> WithDur d MidiMusic
midi' rn = WithDur $ \d -> unWithDur (midi $ unWithDur rn d) d

midiSyncFile :: Ticks -> [WithDur Dur MidiMusic] -> MidiFile
midiSyncFile ticks =
 let dur = ((4 * fromIntegral ticks) % 1)
 in  MidiSync ticks . map (runMidiMusic . (flip unWithDur dur))

-- | A convenient function to write a set of midi notations to a synchronous MIDI-file.
--   NOTE: For unknown reasons not any Ticks value seemes to work. This function uses
--   96 Ticks per quarter.
writeMidiSyncNotation ::  FilePath -> [Notation MidiNote] -> IO ()
writeMidiSyncNotation fp = writeMidiFile fp . midiSyncFile 96 . map (runNotationWith midi)

instance Arrangeable MidiMusic where
 parallel = mergeMidiMusic
 sequent  = appendMidiMusic

instance IsDur d => Musical (WithDur d MidiMusic) where
 rest = WithDur $ \d -> restMidiMusic (fromIntegral $ absDur $ durFrom d)
-------------------------------------------------------------------------------
-- | Calculates the (relative) duration of a 'Notation' (Must be finite!).
durationNotation :: Notation a -> Ratio Int
durationNotation m =
 case m of
  Note d a     -> d
  Rest d       -> d
  m1 :+: m2    -> durationNotation m1   +   durationNotation m2
  m1 :=: m2    -> durationNotation m1 `max` durationNotation m2
  Stretch r m' -> r * durationNotation m'

-- | Calculates the offset for each note.
positionNotation :: Notation a -> Notation (Dur, a)
positionNotation = fst . loop 1 0
 where
 loop l p m = case m of
  Note d a     -> let d' = (d*l) in (Note d' (p, a), p+d')
  Rest d       -> let d' = (d*l) in (Rest d', p+d')
  m1 :+: m2    -> let (m1', p1) = loop l p m1
                      (m2', p2) = loop l p1 m2
                  in (m1' :+: m2', p2)
  m1 :=: m2    -> let (m1', p1) = loop l p m1
                      (m2', p2) = loop l p m2
                  in (m1' :=: m2', max p1 p2)
  Stretch r m' -> loop (r*l) p m'



-- | Reverses a 'Notation' (Must be finite!).
reverseNotation :: Notation a -> Notation a
reverseNotation m =
 case m of
  Note d a     -> m
  Rest d       -> m
  m1 :+: m2    -> reverseNotation m2 :+: reverseNotation m1
  m1 :=: m2    -> let d1 = durationNotation m1
                      d2 = durationNotation m2
                      re | d1 < d2   = (Rest (d2 - d1) :+: reverseNotation m1) :=: reverseNotation m2
                         | d1 > d2   = reverseNotation m1 :=: (Rest (d1 - d2) :+: reverseNotation m2)
                         | otherwise = reverseNotation m1 :=: reverseNotation m2
                  in re
  Stretch r m' -> Stretch r $ reverseNotation m'


-- | Takes the beginning of 'Notation', result has the given duration if possible or is shorter otherwise.
--   Notes that overlap with the end of duration are not taken but replaced by the (fitted) rests.
takeNotation :: Ratio Int -> Notation a -> Notation a
takeNotation len mus = fst $ take len mus
 where
  take :: Ratio Int -> Notation a -> (Notation a, Ratio Int)
  take r m = if r <= 0 % 1 then (Rest (0%1), 0%1) else
   case m of
    Note d a      | d > r     -> (Rest r, 0%1)
                  | otherwise -> (Note d a, r - d)
    Rest d        | d > r     -> (Rest r, 0%1)
                  | otherwise -> (Rest d, r - d)
    m1 :+: m2    -> let (rm1,r1) = take r m1
                        (rm2,r2) = take r1 m2
                    in if r1 > 0%1 then (rm1 :+: rm2, r2) else (rm1, r1)
    m1 :=: m2    -> let (rm1,r1) = take r m1
                        (rm2,r2) = take r m2
                    in (rm1 :=: rm2, min r1 r2)
    --Stretch 0 _  -> (Rest 0, 0)
    Stretch d m' -> let (mu,le) = take (r / d) m' in (Stretch d mu, le)
  takeline r []     = ([], r)
  takeline r (u:us) =
   if r > 0%1
    then let (u',ur) = take r u
             (ul,rr) = takeline ur us
         in if ur > 0%1 then (u':ul,rr) else ([u'],0%1)
    else ([], 0%1)

-- | Drops the beginning of 'Notation'. Notes that would be split are replaced by fitted rests.
dropNotation :: Ratio Int -> Notation a -> Notation a
dropNotation len mus = either id (const $ Rest (0%1)) $ drop len mus
 where
  drop :: Ratio Int -> Notation a -> Either (Notation a) (Ratio Int)
  drop r m = if r <= 0 % 1 then Left m else
   case m of
    Note d a      | d > r     -> Left $ Rest (d - r)
                  | otherwise -> Right (r - d)
    Rest d        | d > r     -> Left $ Rest (d - r)
                  | otherwise -> Right (r - d)
    m1 :+: m2    -> case drop r m1 of
                     Left m1' -> Left (m1' :+: m2)
                     Right r' -> drop r' m2
    m1 :=: m2    -> case (drop r m1, drop r m2) of
                     (Left m1', Left m2') -> Left (m1' :=: m2')
                     (Left m1', _       ) -> Left m1'
                     (_'      , Left m2') -> Left m2'
                     (Right r1, Right r2) -> Right (min r1 r2)
    Stretch d m' -> either (Left . Stretch d) Right $ drop (r / d) m'
  dropline r []     = Right r
  dropline r (u:us) =
   if r > 0%1
    then case drop r u of
          Left u'  -> Left (u' : us)
          Right r' -> dropline r' us
    else Left (u:us)

-- -- | Composes the notations sequentially.
--lineNotation :: [Notation a] -> Notation a
--lineNotation ns = foldr (:+:) (Rest 0) ns

-- -- | Composes the notations in parallel (starting at the same time).
--chordNotation :: [Notation a] -> Notation a
--chordNotation ns = foldr (:=:) (Rest 0) ns

-- | Replaces notes where the predicate fails with rests.
--filterNotation :: (a -> Bool) -> Notation a -> Notation a
filterNotation :: (Musical (m a), Monad m) => (a -> Bool) -> m a -> m a
filterNotation p n = n >>= \a -> if p a then return a else rest

filterNotation' :: (Musical (m a), Musical (m b), Monad m) => (a -> Maybe b) -> m a -> m b
filterNotation' f n = n >>= maybe rest return . f

-- -- | Composes the notations sequentially and divides the duration by the
-- --   number of notations in the list.
--lineNotation' :: [Notation a] -> Notation a
--lineNotation' ns = stretch (1 % length ns) $ lineNotation ns

-- -- | Composes the notations sequentially and stretches them proportionally.
--propNotation :: (Int, Int) -> Notation a -> Notation a -> Notation a
--propNotation (l,r) c1 c2 | l == r    = stretch (1 % 2) (c1 :+: c2)
--                         | otherwise = let l' = abs l
--                                           r' = abs r
--                                       in stretch (l' % (l'+r')) c1
--                                          :+: stretch (r' % (l'+r')) c2

---------------------------------------------------------------------------------------------------
-- | A parallel composition of a sequence of values and a Notation
--   Each value of the sequence has the same given duration.
--   Every Note is updated by a function that gets the actual value of the sequence.
--   NOTE: This function is not tested yet!
sequenceNotation :: (a -> b -> c) -> Dur -> [a] -> Notation b -> Notation c
sequenceNotation f stpsz input mub = let (x,_,_,_) = loop (denominator stpsz % numerator stpsz) (0%1) input mub in x
 where
  step x = let i = div (numerator x) (denominator x)
           in (i, x - (i % 1))
--        stepsize     error                                    duration   error
--loop :: Ratio Int -> Ratio Int -> [a] -> Music b -> (Music c, Ratio Int, Ratio Int, [a])
  loop stepsize off as@(a:_) mu =
   case mu of
    Note d b    -> let (i,off') = step (off + stepsize * d)
                   in (Note d (f a b), d, off', drop i as)
    Rest d      -> let (i,off') = step (off + stepsize * d)
                   in (Rest d, d, off', drop i as)
    m1 :+: m2   -> let (m1', d1, o1, as1) = loop stepsize off as m1
                       (m2', d2, o2, as2) = loop stepsize o1 as1 m2
                   in (m1' :+: m2', d1 + d2, o2, as2)

    m1 :=: m2   -> let (m1', d1, o1, as1) = loop stepsize off as1 m1
                       (m2', d2, o2, as2) = loop stepsize off as2 m2
                       (d', o', as') | d1 < d2   = (d2, o2, as2)
                                     | d1 > d2   = (d1, o1, as1)
                                     | o1 < o2   = (d2, o2, as2)
                                     | otherwise = (d1, o1, as1)
                   in (m1' :=: m2', d', o', as')
    Stretch r m -> let (m', d', o', as') = loop (stepsize * r) off as m
                   in (Stretch r m', d', o', as')
  loop stepsize off [] mu = (Rest (0%1), 0%1, 0%1, [])
---------------------------------------------------------------------------------------------------
-- | NOTE: This function is not tested yet!
sequenceNotations :: (a -> b -> c) -> Dur -> [a] -> [Notation b] -> [Notation c]
sequenceNotations f stpsz input mubs = let (x,_,_,_) = aux (denominator stpsz % numerator stpsz) (0%1) input mubs in x
 where
  step x = let i = div (numerator x) (denominator x)
           in (i, x - (i % 1))
--        stepsize     error                                    duration   error
--loop :: Ratio Int -> Ratio Int -> [a] -> Music b -> (Music c, Ratio Int, Ratio Int, [a])
  loop stepsize off as@(a:_) mu =
   case mu of
    Note d b    -> let (i,off') = step (off + stepsize * d)
                   in (Note d (f a b), d, off', drop i as)
    Rest d      -> let (i,off') = step (off + stepsize * d)
                   in (Rest d, d, off', drop i as)
    m1 :+: m2   -> let (m1', d1, o1, as1) = loop stepsize off as m1
                       (m2', d2, o2, as2) = loop stepsize o1 as1 m2
                   in (m1' :+: m2', d1 + d2, o2, as2)

    m1 :=: m2   -> let (m1', d1, o1, as1) = loop stepsize off as1 m1
                       (m2', d2, o2, as2) = loop stepsize off as2 m2
                       (d', o', as') | d1 < d2   = (d2, o2, as2)
                                     | d1 > d2   = (d1, o1, as1)
                                     | o1 < o2   = (d2, o2, as2)
                                     | otherwise = (d1, o1, as1)
                   in (m1' :=: m2', d', o', as')
    Stretch r m -> let (m', d', o', as') = loop (stepsize * r) off as m
                   in (Stretch r m', d', o', as')
  loop stepsize off [] mu = (Rest (0%1), 0%1, 0%1, [])
  aux stepsize off as@(a:_) ms =
                   let fu o as []     = ([] ,0%1,o,as)
                       fu o as (m:ms) = let (m' ,d,o',as') = loop stepsize o as m
                                            (ms',ds,o'',as'') = fu o' as' ms
                                        in (m':ms', d+ds, o'', as'')
                       (ms',d,off',as') = fu off as ms
                   in (ms', d, off', as')


