module GraphTheory.Algorithm.EdgeColor where
import GraphTheory.Graph (Edge (..), Graph, vertices, edges, incidentEdges, degreeMap, highestDegree, adjacencyList, modifyEdgeUnsafe)
import Data.List ((\\), intersect)
import qualified Data.Map as M
import Debug.Trace (trace)


{-----====== Setup ======-----}
deltaPlusOneColor :: (Ord v, Eq w, Eq v, Show w, Show v) => Graph v w Integer -> Graph v w Integer
deltaPlusOneColor g = inductLoop g g'
    where g' = g {vertices = [] , edges = []}

inductLoop :: (Ord v, Eq w, Eq v, Show w, Show v) => Graph v w Integer -> Graph v w Integer -> Graph v w Integer
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

{-----====== Substance (in sort-of execution order) ======-----}
doDeltaPlusOneColor :: (Ord v, Eq w, Show w, Show v) => Graph v w Integer -> Graph v w Integer
doDeltaPlusOneColor g = colored_g
    where colored_g = if (not . null) missingAtBoth 
                      then easyColor g (head missingAtBoth) 
                      else hardColor g (head $ edges g) gColors
          missingAtBoth = intersect (missingAt v0) (missingAt v1)
          (v0,v1) = endpoints . head $ edges g
          missingAt = colorsMissingAt g gColors
          gColors = colorList g

easyColor :: Graph v w Integer -> Integer -> Graph v w Integer
easyColor g c = g {edges = (new_e:es)}
    where (e:es) = edges g
          new_e = e {edgeData = c}

hardColor :: (Ord v, Eq v, Eq w, Show v, Show w) => Graph v w Integer -> Edge v w Integer -> [Integer] -> Graph v w Integer
hardColor g e gColors = colorSequence
    where (v0,v1) = endpoints e
          presentAt = colorsPresentAt g gColors
          missingAt = colorsMissingAt g gColors
          c0 = head $ intersect (missingAt v0) (presentAt v1)
          c1 = head $ intersect (missingAt v1) (presentAt v0)
          colorSequence = makeColorSequence g gColors [c0,c1] [(v0,v1)]

data SequenceTermination = Branch1 | Branch2
data SequenceResult a = SequenceResult {termReason :: SequenceTermination, 
                                        colorSeq :: [Integer], edgeSeq :: [(a, a)]}

makeColorSequence :: (Ord v, Eq v, Eq w, Show v, Show w) => Graph v w Integer -> [Integer] ->  [Integer] -> [(v,v)] -> Graph v w Integer --SequenceResult v
makeColorSequence g gColors cSeq eSeq 
    | null $ vertexWithColor c_i = trace ((show g) ++ "\n\nCASE 1!!!\n" ++ (show cSeq) ++ " " ++ (show eSeq) ++ " " ++ (show $ vertexWithColor c_i)) $ 
                                   hcCase1 g gColors cSeq eSeq
    | elem (head $ vertexWithColor c_i) (map fst eSeq) = error "CASE 2"
    | otherwise = trace ((show g) ++ "\n\nRECURSE!!!\n" ++ (show cSeq) ++ " " ++ (show eSeq) ++ " " ++ (show $ vertexWithColor c_i)) $ 
                  makeColorSequence g gColors next_cSeq next_eSeq 
    where c_i = last cSeq
          v0 = (fst . head) eSeq                        --TODO This won't work on directed graphs.
          vi_plus_1 = head (vertexWithColor c_i)
          vertexWithColor c = take 1 $ map (otherVertex v0 . endpoints) $ filter (\e -> edgeData e == c) $ incidentEdges g v0
          next_cSeq = cSeq ++ [(head $ colorsMissingAt g gColors vi_plus_1)]
          next_eSeq = eSeq ++ [(v0, vi_plus_1)]

hcCase1 :: (Show v, Show w, Eq v, Eq w) => Graph v w Integer -> [Integer] -> [Integer] -> [(v,v)] -> Graph v w Integer
hcCase1 g gColors cSeq eSeq = foldl recolor g (zip eSeq (tail cSeq)) --Use tail so that C0 doesn't match up iwith (v0, v1).
    where buildEdge (v1,v2) cn = Edge (v1,v2) 1 cn
          recolor gr (e,c) = modifyEdgeUnsafe gr e (\e -> e { edgeData = c})
          

{-----====== Helpers ======-----}
otherVertex :: (Eq v) => v -> (v,v) -> v
otherVertex v (v1,v2) = if v == v1 then v2 else v1

colorList :: (Ord v) => Graph v w Integer -> [Integer]
colorList g = [1..highestDegree g + 1]

colorsMissingAt :: (Ord v, Eq c) => Graph v e c -> [c] -> v -> [c]
colorsMissingAt g gColors v = gColors \\ (map edgeData $ incidentEdges g v)

colorsPresentAt :: (Ord v, Eq c) => Graph v e c -> [c] -> v -> [c]
colorsPresentAt g gColors v = intersect gColors (map edgeData $ incidentEdges g v)
