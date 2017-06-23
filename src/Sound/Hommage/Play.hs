module Sound.Hommage.Play
 (
 -- * Play
   Play (..)
 , getDur
 , resetDur
 , randomPlay
 , mix
 , mix'

 -- * Interpretation of Notation for Play
 , notationMono
 , notationStereo
 , notation
 , Mixable (..)
-- , MixableL

 -- * Parallel played and hierarchical defined tracks
 , Song (..)
 , runSong

 , Track (..)
 , playTrack
 , track
-- , tracks
-- , (-/-)
-- , (-|-)
-- , (-||-)
 , Trackable (..)
-- , notationTracks
-- , ToTrack (..)

 -- * Environment
 , Environment (..)
 , emptyEnvironment
 , nextEnvironment
 , insertEnvironment
 , lookupEnvironment
-- , defineEnvironment

 -- * Duration
 , Duration (..)
 , resetDuration
 , initDuration

 )

 where

import Sound.Hommage.Notation
import Sound.Hommage.Signal
import Sound.Hommage.Seq
import Sound.Hommage.Misc
--import Sound.Hommage.MList

import Data.Ratio
import Data.Dynamic

import Data.IORef
import System.IO.Unsafe
import System.Random

-------------------------------------------------------------------------------
newtype Environment = Environment ( Int , [(Int, [Dynamic])] )

emptyEnvironment :: Environment
emptyEnvironment = Environment (0, [])

nextEnvironment :: Environment -> Environment
nextEnvironment (Environment (c, b)) =
 let b' = loop b in seq b' (Environment (c, b'))
  where
   loop []         = []
   loop ((n,(_:d)):xs) = let xs' = loop xs
                         in seq d (seq xs' ((n,d):xs'))

insertEnvironment :: [Dynamic] -> Environment -> (Environment -> Dynamic, Environment)
insertEnvironment d (Environment (c,b)) =
 seq c (lookupEnvironment c, Environment (c+1, (c,d):b))

lookupEnvironment :: Int -> Environment -> Dynamic
lookupEnvironment n (Environment (_,b)) = case lookup n b of Just (d:_) -> d
-------------------------------------------------------------------------------
class Typeable a => Trackable a where
 toNext :: a -> a

--instance Typeable Signal where
-- --typeOf _ = mkTyConApp (mkTyCon "Signal") []
-- typeRep _ = mkTyConApp (mkTyCon "Signal") []

instance Trackable Signal where
 toNext s = case s of
  Mono (x:xs)   -> seq x (Mono xs)
  Mono []       -> Mono []
  Stereo (x:xs) -> seq x (Stereo xs)
  Stereo []     -> Stereo []

instance Typeable a => Trackable [a] where
 toNext (x:xs) = seq x xs
 toNext []     = []

--instance Typeable Stereo where
-- typeRep _ = mkTyConApp (mkTyCon "Stereo") []
-------------------------------------------------------------------------------
defineEnvironment :: Trackable a => (Environment -> a) -> Environment -> (Environment -> a, Environment)
defineEnvironment f env = (\e -> case fromDynamic $ lkup e of Just a -> a, env')
 where
  iter a = seq a (toDyn a : iter (toNext a))
  (lkup, env') = insertEnvironment (iter $ f env) env

defineEnvironment_ :: Trackable a
                   => a -> Environment -> (Environment -> a, Environment)
defineEnvironment_ a env = (\e -> case fromDynamic $ lkup e of Just a -> a, env')
 where
  iter a = seq a (toDyn a : iter (toNext a))
  (lkup, env') = insertEnvironment (iter a) env

-- (a -> a) -> Environment -> Environment,
-------------------------------------------------------------------------------
data Duration = DURATION
 { relativeDuration :: Dur
 , absoluteDuration :: Dur
 }

instance IsDur Duration where
 durFrom = relativeDuration
 durUpdate f d = d { relativeDuration = f (relativeDuration d) }

resetDuration :: Duration -> Duration
resetDuration d = d { relativeDuration = absoluteDuration d }

initDuration :: Dur -> Duration
initDuration d = DURATION d d
-------------------------------------------------------------------------------
newtype Play a = PLAY { unPlay :: Duration -> Environment -> a }

playToWithDur :: Play a -> WithDur Duration (Environment -> a)
playToWithDur (PLAY p) = WithDur p

--returnPlay :: a -> Play a
--returnPlay a = PLAY $ \dur env -> case env of
-- Environment (0, []) -> a
-- Environment _       -> a

instance Applicative Play where
  pure a = PLAY $ \dur env -> a
  PLAY g <*> PLAY h = PLAY $ \dur env -> g dur env (h dur env)

instance Monad Play where
 return a = PLAY $ \dur env -> a --returnPlay a
 PLAY g >>= f = PLAY $ \dur env -> unPlay (f $ g dur env) dur env

instance Functor Play where
 fmap f (PLAY g) = PLAY $ \dur env -> f (g dur env)
-- fmap f p = p >>= return . f

instance Stretchable (Play a) where
 stretch d (PLAY g) = PLAY $ \duration e -> g (durUpdate (*d) duration) e

getDur :: Play Dur
getDur = PLAY $ \dur _ -> durFrom dur

resetDur :: Play a -> Play a
resetDur (PLAY g) = PLAY $ \d e -> g (resetDuration d) e

randomPlay :: Random a => (a,a) -> Play a
randomPlay v = PLAY $ \_ _ -> unsafePerformIO $ randomRIO v
-----------------------------------------------------------
mix :: [Play Signal] -> Play Signal
mix ps = sequence ps >>= return . mergeSignals

mix' :: Num a => [Play [a]] -> Play [a]
mix' ps = sequence ps >>= return . mergeSet sum
-------------------------------------------------------------------------------
class Mixable a where
 mixdown :: Seq a -> a

instance Num a => Mixable [a] where
 mixdown = mixdownNumSeq

notation :: Mixable a => Notation (Play a) -> Play a
notation n = PLAY $ \dur env -> mixdown $ applySeq nextEnvironment
 (unWithDur (runNotation (fmap (noteSeq' . playToWithDur ) n)) dur) env

notationMono :: Notation (Play Signal) -> Play [Mono]
notationMono n = PLAY $ \dur env -> mixdown $ applySeq nextEnvironment
 (unWithDur (runNotation (fmap (noteSeq' . playToWithDur . fmap signalToMono) n)) dur) env

notationStereo :: Notation (Play Signal) -> Play [Stereo]
notationStereo n = PLAY $ \dur env -> mixdown $ applySeq nextEnvironment
 (unWithDur (runNotation (fmap (noteSeq' . playToWithDur . fmap signalToStereo) n)) dur) env

-------------------------------------------------------------------------------
data Song a = SONG { unSong :: Duration -> Environment -> (a, Environment) }

instance Functor Song where
 fmap f (SONG g) = SONG $ \d e -> let (a, e') = g d e in (f a, e')

instance Applicative Song where
  pure a = SONG $ \dur env -> (a, env)
  SONG g <*> SONG h = SONG $ \dur env ->
    let (f, e') = g dur env
        (a, e'') = h dur e'
    in
    (f a, e'')

instance Monad Song where
 return a = SONG $ \_ e -> (a,e)
 SONG g >>= f = SONG $ \d e -> let (a,e') = g d e in unSong (f a) d e'

runSong :: Double -> Song (Play a) -> a
runSong bpm (SONG ts) =
 let duration = initDuration $ bpmToDur bpm
     (PLAY p, env) = ts duration emptyEnvironment
 in p duration env
-------------------------------------------------------------------------------
newtype Track a = TRACK { unTrack :: Environment -> a }

instance Functor Track where
 fmap f (TRACK g) = TRACK (f . g)

playTrack :: Track a -> Play a
playTrack (TRACK get) = PLAY $ \d e -> get e

track :: Trackable a => Play a -> Song (Track a)
track (PLAY p) = SONG $ \dur env ->
 let (get, env') = defineEnvironment (p dur) env
 in seq env' (TRACK get, env')
-------------------------------------------------------------------------------
{- Causes too much memory use. Reason: More than one Reference to Environment.

notationTracks :: (ToFilter a l, MixableL l) => Notation (Play a) -> Song (l Track)
notationTracks n = SONG $ \dur env ->
 let f = toFilter (undefined :: a)
     (l, env') = mixdownL (applySeq nextEnvironment (runNotation (fmap (noteSeq' . unPlay) n) dur) env) f env
 in seq env' (l, env')

aux1 :: (ToFilter a l, MixableL l) => Notation (Play a) -> Seq a -> l (Filter a) -> Environment -> (l Track, Environment)
aux1 _ = mixdownL

-------------------------------------------------------------------------------
class MixableL l where
 mixdownL :: Seq a -> l (Filter a) -> Environment -> (l Track, Environment)

instance MixableL NilM where
 mixdownL sq NilM env = (NilM, env)

instance (Mixable a, Trackable a, MixableL b) => MixableL (LM a b) where
 mixdownL sq (a :>> b) env =
  let (get, env') = defineEnvironment_ (mixdown $ filterSeq (unFilter a) sq) env
      (b', env'') = mixdownL sq b env'
  in seq env' (seq env' (TRACK get :>> b', env''))
-------------------------------------------------------------------------------


infixr 5 -|-, -||-


(-/-) :: Trackable b => Song a -> Play b -> Song (L a (Track b))
SONG s -/- PLAY p = SONG $ \dur env ->
 let ~(a, env') = s dur env
     ~(get, env'') = defineEnvironment_ (p dur env) env'
 in seq env' (seq env'' ((a :> TRACK get), env''))


(-|-) :: Trackable a => Play a -> Song b -> Song (L (Track a) b)
PLAY p -|- SONG s = SONG $ \dur env ->
 let ~(b, env') = s dur env
     ~(get, env'') = defineEnvironment_ (p dur env) env'
 in seq env' (seq b (seq env''(seq get (TRACK get :> b, env''))))

(-||-) :: Trackable a => Play a -> Song (b Track) -> Song (LM a b Track)
PLAY p -||- SONG s = SONG $ \dur env ->
 let (b, env') = s dur env
     (get, env'') = defineEnvironment_ (p dur env) env'
 in seq env' $ seq env'' ((TRACK get :>> b), env'')

class ToTrack l where
-- toTrack :: l Play -> Duration -> Environment -> Environment -> (l Track, Environment)
 toTrack :: l Play -> Duration -> Environment -> (l Track, Environment)

instance ToTrack NilM where
-- toTrack NilM _ _ env' = (NilM, env')
 toTrack NilM _ env = (NilM, env)

--instance (Trackable a, ToTrack b) => ToTrack (LM a b) where
-- toTrack (PLAY a :>> b) dur env env' = let (get, env'') = defineEnvironment_ (a dur env) env'
--                                           (b', env''') = toTrack b dur env env''
--                                       in seq env (seq env' (seq env'' (seq env''' (TRACK get :>> b', env'''))))

instance (Trackable a, ToTrack b) => ToTrack (LM a b) where
 toTrack (PLAY a :>> b) dur env = case toTrack b dur env of
                                   ~(b', env') -> case seq b' (seq env' (defineEnvironment_ (a dur env) env')) of
                                    ~(get, env'') -> seq get (seq env'' (TRACK get :>> b', env''))

tracks :: ToTrack l => l Play -> Song (l Track)
tracks l = SONG $ \dur env -> toTrack l dur env

-- l Play -> Song (l Track)
-}
-------------------------------------------------------------------------------

-- notations :: Notation (Play a) -> Play



-- Seq e -> l (Filter e) -> l Identity







