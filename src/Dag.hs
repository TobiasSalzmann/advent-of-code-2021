module Dag where

import           Data.Map   (Map, (!), (!?))
import qualified Data.Map   as M
import           Data.Maybe (catMaybes, isNothing)

runRecursion ::
     (Ord a, Eq a) => (a -> Maybe v) -> ([v] -> v) -> (a -> [a]) -> a -> v
runRecursion fBase fComb g start =
  (runRecursion' fBase fComb g [start] M.empty) ! start

runRecursion' ::
     (Ord a, Eq a)
  => (a -> Maybe v)
  -> ([v] -> v)
  -> (a -> [a])
  -> [a]
  -> Map a v
  -> Map a v
runRecursion' _ _ _ [] m = m
runRecursion' fBase fComb g (x:xs) m =
  case M.lookup x m of
    Just _ -> runRecursion' fBase fComb g xs m
    Nothing ->
      case fBase x of
        Just v -> runRecursion' fBase fComb g xs $ M.insert x v m
        Nothing ->
          let deps = g x
              vals = map (m !?) deps
           in case any isNothing vals of
                False ->
                  runRecursion' fBase fComb g xs $
                  M.insert x (fComb . catMaybes $ vals) m
                True -> runRecursion' fBase fComb g (deps ++ (x : xs)) m

countPaths :: (Ord a, Eq a, Integral v) => (a -> Bool) -> (a -> [a]) -> a -> v
countPaths fBase =
  runRecursion
    (\x ->
       if fBase x
         then Just 1
         else Nothing)
    sum
