module Grasph.Algorithm.DFS where

import Grasph.Graph
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

-- This depth-first search is strange, bad, and (probably) slow. It works well
-- enough for now.
-- TODO: do this better

getComponents :: (Ord v) => Graph v w c -> [(v,Integer)]
getComponents g = M.toList $ depthFirstSearchSCC g

depthFirstSearchSCC :: (Ord v) => Graph v w c -> M.Map v Integer
depthFirstSearchSCC g = depthFirstSearchSCC' g dfi 1
    where dfi = M.fromList $ map (\v -> (v,0)) $ vertices g

depthFirstSearchSCC' :: (Ord v) => Graph v w c -> M.Map v Integer -> Integer ->  M.Map v Integer
depthFirstSearchSCC' g dfi i
    | (not . null) unsearched = depthFirstSearchSCC' g dfsResult (i+1)
    | otherwise = dfi
    where unsearched = filter (\v -> uLookup dfi v == 0) $ vertices g
          dfsResult = dfsscc g (head unsearched) dfi i
          --components = M.filter (/= 0) dfsResult

dfsscc :: (Ord v) => Graph v w c -> v -> M.Map v Integer -> Integer -> M.Map v Integer
dfsscc g v dfi i
    | null unsearched = new_dfi
    | otherwise = foldl (\m v -> dfsscc g v m i) new_dfi unsearched
    where new_dfi = uUpdate dfi v i
          unsearched = filter (\k -> uLookup new_dfi k == 0) adj
          adj = adjacencyList g v

uUpdate m k v = M.update (\_ -> Just v) k m
uLookup m k = fromJust $ M.lookup k m
uInsert m k v = M.insert k v m
