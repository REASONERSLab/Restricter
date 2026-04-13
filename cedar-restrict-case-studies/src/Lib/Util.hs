module Lib.Util where

import Control.Monad.State.Strict
import Data.Function
import GHC.Stack
import List.Shuffle
import System.Random.Stateful

uniformShuffleList :: RandomGen g => [a] -> State g [a]
uniformShuffleList xs = state (shuffle xs)

modifyAt :: [a] -> Int -> (a -> a) -> [a]
modifyAt xs idx f =
  let (pre, s:suf) = splitAt idx xs in
  pre ++ (f s) : suf

randomElem :: HasCallStack => RandomGen g => [a] -> State g a
randomElem xs = do
  idx <- state $ randomR (0, length xs - 1)
  return $ xs !! idx

-- NOTE: poorly optimized
randomElemsNoRepeat :: RandomGen g => Int -> [a] -> State g [a]
randomElemsNoRepeat n xs = do
  shuff <- uniformShuffleList xs
  return $ shuff & take n
