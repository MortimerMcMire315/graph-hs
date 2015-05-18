module GraphTheory.Algorithm.EdgeColor where
import GraphTheory.Graph (ColoredGraph, c_vertices, c_edges)
import Data.List ((\\))
import Debug.Trace (trace)

color :: (Eq w, Eq v, Eq c, Show w, Show v, Show c) => ColoredGraph v w c -> ColoredGraph v w c
color g = inductLoop g g'
    where g' = g {c_vertices = [u,v] , c_edges = [fstEdge]}
          fstEdge@((u,v),_,c) = head $ c_edges g

inductLoop :: (Eq w, Eq v, Eq c, Show w, Show v, Show c) => ColoredGraph v w c -> ColoredGraph v w c -> ColoredGraph v w c
inductLoop g g'
    | (length . c_edges) g == (length . c_edges) g' = g'
    | otherwise = trace (show $ c_edges g') $ inductLoop g nextg'
    where nextg' = g' {c_edges = g'_edges, c_vertices = g'_vertices}
          edgeToAdd@((v1,v2),_,_) = head $ (c_edges g) \\ (c_edges g')
          g'_edges = edgeToAdd : (c_edges g')
          g'_vertices = foldl vertCheck (c_vertices g') [v1,v2]
                where vertCheck oldVs newV = if elem newV oldVs then oldVs
                                             else newV:oldVs
