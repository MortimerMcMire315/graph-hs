module GraphTheory.Algorithm.EdgeColor where
import GraphTheory.Graph (Graph, vertices, edges)
import Data.List ((\\))
import Debug.Trace (trace)

color :: (Eq e, Eq v, Show e, Show v) => Graph v e -> Graph v e
color g = inductLoop g (g {vertices = fstEdgeEndpoints, edges = [fstEdge]})
    where fstEdge = head $ edges g
          fstEdgeEndpoints = [fst . fst $ fstEdge, snd . fst $ fstEdge]

inductLoop :: (Eq e, Eq v, Show e, Show v) => Graph v e -> Graph v e -> Graph v e
inductLoop g g'
    | (length . edges) g == (length . edges) g' = g'
    | otherwise = trace (show g') $ inductLoop g nextg'
    where nextg' = g' {edges = nextg'_edges, vertices = nextg'_vertices}
          newg'_edge = head $ (edges g) \\ (edges g')
          nextg'_edges = newg'_edge : (edges g')
          nextg'_vertices = foldl vertCheck (vertices g') [v1,v2]
                where v1 = fst . fst $ newg'_edge
                      v2 = snd . fst $ newg'_edge
                      vertCheck oldVs newV = if elem newV oldVs then oldVs
                                             else newV:oldVs
