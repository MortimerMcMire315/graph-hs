module Grasph.Algorithm.MWST where

import Data.List    ( nub
                    , sortBy )
import qualified Data.Map.Strict as M
import Grasph.Graph
import Grasph.Misc.Infinity

kruskal :: (Ord w, Eq w, Eq v, Eq d) => Graph v w d -> Graph v w d
kruskal g = kruskal' sortedEdges (g {vertices = [], edges = []})
    where sortedEdges = sortBy (\e1 e2 -> compare (weight e1) (weight e2)) $ edges g

kruskal' :: (Eq w, Eq v, Eq d) => [Edge v w d] -> Graph v w d -> Graph v w d
kruskal' [] mwt = mwt
kruskal' (e:es) mwt = if acyclic mwtWithEdge
                      then kruskal' es mwtWithEdge
                      else kruskal' es mwt
    where mwtWithEdge = addEdgeAndVerts mwt e
          acyclic g = length (vertices g) == length (edges g) + 1

prim :: (Ord w) => Graph v w d -> Graph v w d
prim g = g {edges = prim' g g [] (lengthmap g)}

prim' :: (Ord w) => [v] -> [Edge v w d] -> Int -> M.Map v (Infinitable w)
prim' v v' t lv
    | length v' == n = t
    |

lengthmap :: (Ord w) => Graph v w d -> v -> M.Map v (Infinitable w)
lengthmap g v = fromList . map f $ edges g
    where f edge =
          adj = adjacencyList g v
