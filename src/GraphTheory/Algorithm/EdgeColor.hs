module GraphTheory.Algorithm.EdgeColor where
import GraphTheory.Graph (Edge (..), Graph, vertices, edges)
import Data.List ((\\))
import qualified Data.Map as M
import Debug.Trace (trace)

deltaPlusOneColor :: (Eq w, Eq v, Eq c, Show w, Show v, Show c) => Graph v w c -> Graph v w c
deltaPlusOneColor g = inductLoop g g'
    where g' = g {vertices = [u,v] , edges = [fstEdge]}
          fstEdge = head $ edges g
          (u,v) = endpoints fstEdge

inductLoop :: (Eq w, Eq v, Eq c, Show w, Show v, Show c) => Graph v w c -> Graph v w c -> Graph v w c
inductLoop g g'
    | (length . edges) g == (length . edges) g' = g'
    | otherwise = trace (show $ edges g') $ inductLoop g nextg'
    where nextg' = g' {edges = g'_edges, vertices = g'_vertices}
          edgeToAdd@(Edge (v1,v2) _ _) = head $ (edges g) \\ (edges g')
          g'_edges = edgeToAdd : (edges g')
          g'_vertices = foldl vertCheck (vertices g') [v1,v2]
                where vertCheck oldVs newV = if elem newV oldVs then oldVs
                                             else newV:oldVs

--colorsMissingAt :: (Ord v) => Graph v e c -> v -> Integer -> [c]
--colorsMissingAt g v Δ = allColors \\ (map getColor )
--    where allColors = [1..Δ]

highestDegree :: (Ord v) => Graph v w c -> Integer
highestDegree g = maximum $ (map snd) $ M.toList $ degreeMap g

degreeMap :: (Ord v) => Graph v w c -> M.Map v Integer
degreeMap g = degreeMap' (edges g) $ M.fromList [(v,0) | v <- vertices g]

degreeMap' :: (Ord v) => [Edge v w c] -> M.Map v Integer -> M.Map v Integer
degreeMap' [] m = m
degreeMap' (e:es) m = degreeMap' es new_m
    where (v1,v2) = endpoints e
          new_m = M.update updateF v1 $ M.update updateF v2 m
          updateF x = Just $ x + 1

