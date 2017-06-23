module Sound.Hommage.Misc
 (
 -- * Imperative control structure
   for
 , for'

 -- * Usefull list functions
 , map_
 , foldr_
 , merge
 , mergeSet
 , uneitherlist

 , walk
 , appendmaps
 , appendmaps'

 , qsort
 , qsortM

 -- * More usefull functions
 , head_opt
 , newFilePath

 -- * Embedding IO in a list (-map)
 , inList
 , inList'
 , inList''
 , toList
 , toList'
 , inListE
 , inListE'
 , inListE''
 , toListE
 , toListE'

 )
 where

import System.Directory
import System.IO.Unsafe
import Data.Int
import Data.List

---------------------------------------------------------------------------------------------------
for :: Monad m => a -> (a -> Bool) -> (a -> a) -> (a -> m b) -> m ()
for count test step fun | test count = fun count >> for (step count) test step fun
                        | otherwise  = return ()

for' :: Monad m => a -> (a -> Bool) -> (a -> a) -> b -> (a -> b -> m b) -> m b
for' count test step state fun | test count = do state' <- fun count state
                                                 for' (step count) test step state' fun
                               | otherwise  = return state
---------------------------------------------------------------------------------------------------
-- | a pseudo-strict 'map' version. does not touch the elements but reconstructs the whole list
--   structure before the function is applied to the value
map_ :: (a -> b) -> [a] -> [b]
map_ f []     = []
map_ f (x:xs) = let r = map_ f xs in seq r (f x : r)

-- | a pseudo-strict 'foldr' version. does not touch the elements but reconstructs the whole list
--   structure  before the function is applied to the values.
foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f e []     = e
foldr_ f e (x:xs) = let r = foldr_ f e xs in seq r (f x r)

-- | Similar to 'zipWith', but the result has the length of the longer input list.
merge :: (a -> a -> a) -> [a] -> [a] -> [a]
merge f = loop
 where
  loop (x:xs) (y:ys) = (f x y) : loop xs ys
  loop xs     []     = xs
  loop []     ys     = ys

mergeSet :: ([a] -> b) -> [[a]] -> [b]
mergeSet f = map f . transpose

mergeSet_ :: ([a] -> b) -> [[a]] -> [b]
mergeSet_ f = strict . map f . transpose

strict :: [a] -> [a]
strict (x:xs) = seq x (x : strict xs)
strict []     = []

head_opt :: a -> [a] -> a
head_opt _ (x:xs) = x
head_opt x _      = x

uneitherlist :: [Either a b] -> ([a],[b])
uneitherlist []            = ([],[])
uneitherlist (Left  a : s) = let (l,r) = uneitherlist s in (a:l,r)
uneitherlist (Right b : s) = let (l,r) = uneitherlist s in (l,b:r)

walk :: ([a] -> [b]) -> [a] -> ([a],[b])
walk f a = loop a (f a)
 where
  loop (a:as) (b:bs) = let (as',bs') = loop as bs in (as',b : bs')
  loop as     []     = (as, [])
  loop []     bs     = ([], bs)

appendmaps :: [[a] -> [b]] -> [a] -> [b]
appendmaps (f:fs) as = let (as', bs) = walk f as in bs ++ appendmaps fs as'
appendmaps _      _  = []

appendmaps' :: [[a] -> [b]] -> [a] -> [[b]]
appendmaps' (f:fs) as = let (as', bs) = walk f as in bs : appendmaps' fs as'
appendmaps' _      _  = []
---------------------------------------------------------------------------------------------------
qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort p i = so i
 where
  so [x]    = [x]
  so (x:xs) = let (l,r) = sp x xs in so l ++ [x] ++ so r
  so []     = []
  sp x (y:ys) = let (l,r) = sp x ys in if p y x then (y:l,r) else (l,y:r)
  sp x []     = ([],[])


qsortM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
qsortM p = sort
 where
  sort [x]    = return [x]
  sort (x:xs) = do (l,r) <- split x xs
                   l'    <- sort l
                   r'    <- sort r
                   return (l' ++ (x : r'))
  sort []     = return []

  split y (x:xs) = do (l,r) <- split y xs
                      b <- p x y
                      if b then return (x:l,r) else return (l,x:r)
  split y []     = return ([],[])
---------------------------------------------------------------------------------------------------
inList :: IO (Maybe a -> IO (Maybe b)) -> [a] -> [b]
inList mkf la = unsafePerformIO (mkf >>= \f -> loop f la)
 where
  loop f []    = f Nothing  >>= maybe (return []) (\b -> return (b : unsafePerformIO (loop f [])))
  loop f (a:r) = f (Just a) >>= maybe (return []) (\b -> return (b : unsafePerformIO (loop f r)))

inList' :: IO (a -> IO b) -> [a] -> [b]
inList' mkf la = unsafePerformIO (mkf >>= \f -> loop f la)
 where
  loop f []    = return []
  loop f (a:r) = f a >>= \b -> return (b : unsafePerformIO (loop f r))

inList'' :: IO (a -> IO (Maybe b)) -> [a] -> [b]
inList'' mkf la = unsafePerformIO (mkf >>= \f -> loop f la)
 where
  loop f []    = return []
  loop f (a:r) = f a >>= maybe (return []) (\b -> return (b : unsafePerformIO (loop f r)))

toList :: IO (IO a) -> [a]
toList f = unsafePerformIO (f >>= loop)
 where
  loop m = m >>= \a -> return (a : unsafePerformIO (loop m))

toList' :: IO (IO (Maybe a)) -> [a]
toList' f = unsafePerformIO (f >>= loop)
 where
  loop m = m >>= maybe (return []) (\a -> return (a : unsafePerformIO (loop m)))

---------------------------------------------------------------------------------------------------
inListE :: IO (Either (Maybe a -> IO (Maybe b)) (Maybe a -> IO (Maybe c))) -> [a] -> Either [b] [c]
inListE mkf la = unsafePerformIO (mkf >>= either (\f -> fmap Left $ loop f la) (\f -> fmap Right $ loop f la) )
 where
  loop f []    = f Nothing  >>= maybe (return []) (\b -> return (b : unsafePerformIO (loop f [])))
  loop f (a:r) = f (Just a) >>= maybe (return []) (\b -> return (b : unsafePerformIO (loop f r)))

inListE' :: IO (Either (a -> IO b) (a -> IO c)) -> [a] -> Either [b] [c]
inListE' mkf la = unsafePerformIO (mkf >>= either (\f -> fmap Left $ loop f la) (\f -> fmap Right $ loop f la) )
 where
  loop f []    = return []
  loop f (a:r) = f a >>= \b -> return (b : unsafePerformIO (loop f r))

inListE'' :: IO (Either (a -> IO (Maybe b)) (a -> IO (Maybe c))) -> [a] -> Either [b] [c]
inListE'' mkf la = unsafePerformIO (mkf >>= either (\f -> fmap Left $ loop f la) (\f -> fmap Right $ loop f la) )
 where
  loop f []    = return []
  loop f (a:r) = f a >>= maybe (return []) (\b -> return (b : unsafePerformIO (loop f r)))

toListE :: IO (Either (IO a) (IO b)) -> Either [a] [b]
toListE f = unsafePerformIO (f >>= either (fmap Left . loop) (fmap Right . loop))
 where
  loop m = m >>= \a -> return (a : unsafePerformIO (loop m))

toListE' :: IO (Either (IO (Maybe a)) (IO (Maybe b))) -> Either [a] [b]
toListE' f = unsafePerformIO (f >>= either (fmap Left . loop) (fmap Right . loop))
 where
  loop m = m >>= maybe (return []) (\a -> return (a : unsafePerformIO (loop m)))
-------------------------------------------------------------------------------
-- | the given function must create a filepath using the Int-argument. The filepath must be
--   different for different arguments. 'newFilePath' works like Gödels mu-operator and terminates
--   only if the filepath does not exist.
newFilePath :: (Int -> FilePath) -> IO FilePath
newFilePath filename = loop 0
 where
  loop n = do let fn = filename n
              b <- doesFileExist fn
              if b then loop (n+1) else return fn







