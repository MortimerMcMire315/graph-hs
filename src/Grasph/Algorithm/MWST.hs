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
prim g = g {edges = prim' g [tail $ vertices g] [head $ vertices g] [] (lengthmap g)}

prim' :: (Ord w) => Graph v w d
                 -> [v]                     -- the set (V-V')
                 -> [v]                     -- V'
                 -> [Edge v w d]            -- T
                 -> M.Map v (Infinitable w) -- L(v)
                 -> [Edge v w d]
prim' g unvisited v'@(u:us) t lv
    | length v' == (length $ vertices g) = t
    | otherwise = prim' g unvisited' v'' e:t lv'
    where w = minWeightV [(v, fromJust (M.lookup lv v)) | v <- unvisited]
          e = findEdge (u, w)

lengthmap :: (Ord w) => Graph v w d
                     -> [v]
                     -> v
                     -> M.Map v (Edge v w d, Infinitable w)
lengthmap g vs u = fromList . map f $ vertices g
    where f v= (v, dist g vs v)
          adj = adjacencyList g u

dist :: Graph v w d -> v -> v -> Infinitable w
dist g u v = dist' (allEdges g) u v

-- Find the distance from a set of vertices to a given vertex
dist :: Graph v w d -> [v] -> v -> (v, Infinitable w)
dist g vset v' = dist' (allEdges g) vset v

dist' [] _ _ = PositiveInfinity
dist' es vset v' = tupleSndMin $ map (\v -> dist'' es v v')

dist'' :: [Edge v w d] -> v -> v -> (v, Infinitable w)
dist'' [] _ _ = PositiveInfinity
dist'' (e:es) u v
    | endpoints e == (u,v) = Regular $ weight e
    | otherwise = dist' es u v

tupleSndMin = foldl1 (\t1 t2 -> if (snd t1) < (snd t2) then t1 else t2)
