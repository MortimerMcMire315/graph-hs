module GraphTheory.Algorithm.EdgeColor where
import GraphTheory.Graph (ColoredEdge, ColoredGraph, c_vertices, c_edges)
import Data.List ((\\))
import qualified Data.Map as M
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

--getColor :: ColoredEdge v e c -> c
--getColor (_,_,c) = c

--colorsMissingAt :: (Ord v) => ColoredGraph v e c -> v -> Integer -> [c]
--colorsMissingAt g v Δ = allColors \\ (map getColor )
--    where allColors = [1..Δ]

highestDegree :: (Ord v) => ColoredGraph v e c -> Integer
highestDegree g = maximum $ (map snd) $ M.toList $ degreeMap g

degreeMap :: (Ord v) => ColoredGraph v w c -> M.Map v Integer
degreeMap g = degreeMap' (c_edges g) $ M.fromList [(v,0) | v <- c_vertices g]

degreeMap' :: (Ord v) => [ColoredEdge v w c] -> M.Map v Integer -> M.Map v Integer
degreeMap' [] m = m
degreeMap' (((v1,v2), _, _):es) m = degreeMap' es new_m
    where new_m = M.update updateF v1 $ M.update updateF v2 m
          updateF x = Just $ x + 1
