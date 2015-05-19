module GraphTheory.Algorithm.EdgeColor where
import GraphTheory.Graph (Edge (..), Graph, vertices, edges, incidentEdges, degreeMap, highestDegree)
import Data.List ((\\))
import qualified Data.Map as M
import Debug.Trace (trace)

deltaPlusOneColor :: (Eq w, Eq v, Show w, Show v) => Graph v w Integer -> Graph v w Integer
deltaPlusOneColor g = inductLoop g g'
    where g' = g {vertices = [] , edges = []}

inductLoop :: (Eq w, Eq v, Show w, Show v) => Graph v w Integer -> Graph v w Integer -> Graph v w Integer
inductLoop g gp
    | null $ edges g = gp
    | otherwise = {--trace (show $ edges gp) $--} inductLoop g {edges=restEdges} $ doDeltaPlusOneColor nextgp
    where nextgp = gp {edges = gp_edges, vertices = gp_vertices}
          (firstEdge:restEdges) = edges g
          gp_edges = (firstEdge {edgeData = -1}) : (edges gp)
          gp_vertices = foldl vertCheck (vertices gp) [v1,v2]
                where vertCheck oldVs newV = if elem newV oldVs then oldVs
                                             else newV:oldVs
                      (v1,v2) = endpoints firstEdge

colorList :: (Ord v) => Graph v w Integer -> [Integer]
colorList g = [1..highestDegree g + 1]

colorsMissingAt' :: (Ord v, Eq c) => Graph v e c -> v -> [c] -> [c]
colorsMissingAt' g v gColors = gColors \\ (map edgeData $ incidentEdges g v)

doDeltaPlusOneColor g = g
