module Grasph.Algorithm.MWST where

import Data.List    ( nub
                    , sortBy )
import Grasph.Graph

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
