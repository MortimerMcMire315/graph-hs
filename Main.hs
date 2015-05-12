module Main where

import GraphTheory.Graph
import GraphTheory.SpecialGraphs
import qualified Data.Map as M
import Data.Maybe (fromJust)

g1 :: Graph Integer Integer
g1 = uwGraph [1, 2, 3, 4, 5, 6] [((1,2),0), ((3,2),0),((5,3),6),((4,1),10),((5,6),1)]

--dijkstra :: (Integral e, Bounded e, Ord v) => Graph v e -> [(v,e)]
--dijkstra g = dijkstra' g (startTable g) [u]
--    where u = head $ vertices g
--          startTable = map (\v -> ) $ vertices g
--          adj vert = fromJust $ M.lookup vert fullAdjacency 
--          fullAdjacency = adjacencyListMap g -- This will only be calculated once
--
--adjacencyListMap :: (Ord v, Eq v) => Graph v e -> M.Map v [v]
--adjacencyListMap g = M.fromList $ [(v, adjacencyList g v) | v <- vertices g ]
--
--infTable :: (Integral e, Bounded e, Ord v) => Graph v e -> M.Map v e
--infTable g = foldl (\m v -> M.insert v maxBound m) M.empty (vertices g)
--
--dijkstra' :: (Integral e, Ord v) => Graph v e -> M.Map v e -> [v] -> [(v,e)]
--dijkstra' g l t
--    | M.size l == length (vertices g) = M.toList l
--    | otherwise = [(head $ vertices g, snd . head $ edges g)]
--    where 
main = putStrLn $ show g1
