module GraphTheory.Algorithm.EdgeColor where

import GraphTheory.Graph (Edge (..), Graph, vertices, edges, incidentEdges, degreeMap, highestDegree, adjacencyList, modifyEdgeUnsafe, setEdgeColor)
import GraphTheory.Algorithm.DFS (getComponents)
import Data.List ((\\), intersect, elemIndex, nub)
import qualified Data.Map as M
import GraphTheory.Misc.Infinity (Inf, Infinitable (..))
import Data.Maybe (fromJust)


{-----====== Setup ======-----}
deltaPlusOneColor :: (Ord v, Eq w, Eq v) => 
                     Graph v w Integer -> Graph v w Integer
deltaPlusOneColor g = inductLoop g g'
    where g' = g {vertices = [] , edges = []}

inductLoop :: (Ord v, Eq w, Eq v) => 
              Graph v w Integer -> Graph v w Integer -> Graph v w Integer
inductLoop g gp
    | null $ edges g = gp
    | otherwise = inductLoop g {edges=restEdges} $ doDeltaPlusOneColor nextgp
    where nextgp = gp {edges = gp_edges, vertices = gp_vertices}
          (firstEdge:restEdges) = edges g
          gp_edges = (firstEdge {edgeData = -1}) : (edges gp)
          gp_vertices = foldl vertCheck (vertices gp) [v1,v2]
                where vertCheck oldVs newV = if elem newV oldVs then oldVs
                                             else newV:oldVs
                      (v1,v2) = endpoints firstEdge


{-----====== Substance (in sort-of execution order) ======-----}
doDeltaPlusOneColor :: (Ord v, Eq w) => 
                       Graph v w Integer -> Graph v w Integer
doDeltaPlusOneColor g = if (not . null) missingAtBoth 
                        then easyColor g (head missingAtBoth) 
                        else hardColor g (head $ edges g) gColors
    where missingAtBoth = intersect (missingAt v0) (missingAt v1)
          (v0,v1) = endpoints . head $ edges g
          missingAt = colorsMissingAt g gColors
          gColors = colorList g


easyColor :: Graph v w Integer -> Integer -> Graph v w Integer
easyColor g c = g {edges = (new_e:es)}
    where (e:es) = edges g
          new_e = e {edgeData = c}

hardColor :: (Ord v, Eq v, Eq w) => 
             Graph v w Integer -> Edge v w Integer -> [Integer] -> Graph v w Integer
hardColor g e gColors = colorSequence
    where (v0,v1) = endpoints e
          presentAt = colorsPresentAt g gColors
          missingAt = colorsMissingAt g gColors
          c0 = head $ intersect (missingAt v0) (presentAt v1)
          c1 = head $ intersect (missingAt v1) (presentAt v0)
          colorSequence = makeColorSequence g gColors [c0,c1] [(v0,v1)]


makeColorSequence :: (Ord v, Eq v, Eq w) => 
                     Graph v w Integer -> [Integer] ->  [Integer] -> [(v,v)] -> Graph v w Integer
makeColorSequence g gColors cSeq eSeq 
    | null $ vertexWithColor c_i = hcCase1 g cSeq eSeq
    | elem vk (map snd eSeq) = hcCase2 g cSeq eSeq vk
    | otherwise = makeColorSequence g gColors next_cSeq next_eSeq 
    where c_i = last cSeq
          v0 = (fst . head) eSeq
          vi_plus_1 = head (vertexWithColor c_i)
          vertexWithColor c = take 1 $ map (otherVertex v0 . endpoints) $ filter (\e -> edgeData e == c) $ incidentEdges g v0
          vk = head $ vertexWithColor c_i
          next_cSeq = cSeq ++ [(head $ colorsMissingAt g gColors vi_plus_1)]
          next_eSeq = eSeq ++ [(v0, vi_plus_1)]

hcCase1 :: (Eq v, Eq w) => 
           Graph v w Integer -> [Integer] -> [(v,v)] -> Graph v w Integer
hcCase1 g cSeq eSeq = foldl setEdgeColor g (zip eSeq (tail cSeq)) --Use tail so that C0 doesn't match up with (v0, v1).
    where buildEdge (v1,v2) cn = Edge (v1,v2) 1 cn

hcCase2 :: (Ord v, Eq v, Eq w) => 
           Graph v w Integer -> [Integer] -> [(v,v)] -> v -> Graph v w Integer
hcCase2 g cSeq eSeq vk = kempeBranch recoloredG cSeq eSeq vk
    where recoloredG = setEdgeColor (setEdgeColors g zipped) ((v0,vk),-1)
          v0 = (fst . head) eSeq
          vj = (snd . last) eSeq
          eSeqBeforevk = takeWhile (\x -> snd x /= vk) eSeq
          zipped = zip eSeqBeforevk $ tail cSeq --Edges to recolor in the first step, paired with their new colors


kempeBranch :: (Ord v, Eq v, Eq w) => 
               Graph v w Integer -> [Integer] -> [(v,v)] -> v -> Graph v w Integer
kempeBranch g cSeq eSeq vk = if notElem v0 (map fst $ h vk) && notElem v0 (map snd $ h vk)
                             then branchA g (h vk) c0 cj v0 vk
                             else branchB g (h vj) c0 cj cSeq eSeq v0 vk
    where v0 = (fst . head) eSeq; vj = (snd . last) eSeq 
          c0 = head cSeq; cj = last cSeq
          h = kempeCompEdges g c0 cj
          h_vk = h vk; h_vj = h vj;

--Return the edges in the kempe component characterized by the given vertex
kempeCompEdges :: (Ord v, Eq v, Eq w) => 
                  Graph v w Integer -> Integer -> Integer -> v -> [(v,v)]
kempeCompEdges g c0 cj vert = map endpoints $ filter kempeFilter (edges kempe)
    where kempeFilter e = let k = kempeCompVertices vert; (v1,v2) = endpoints e 
                          in (elem v1 k && elem v2 k)
          kempeCompVertices vert = map fst $ filter (\t -> snd t == componentIndex vert) kempeComponents
          componentIndex vert = snd . head $ filter (\t -> fst t == vert) kempeComponents 
          kempeComponents = getComponents kempe
          kempe = kempeSubgraph g c0 cj

                       
branchA g hvk c0 cj v0 vk = setEdgeColor (interchangeColors g hvk c0 cj) ((v0,vk),c0)

branchB :: (Eq v, Eq w) => Graph v w Integer -> [(v,v)] -> Integer -> Integer -> [Integer] -> [(v,v)] -> v -> v -> Graph v w Integer
branchB g hvj c0 cj cSeq eSeq v0 vk = setEdgeColor interchanged_g ((v0,vj),c0)
    where k = fromJust $ elemIndex vk (map snd eSeq)
          vj = (snd . last) eSeq
          cSeq_end = init $ drop (k + 1) cSeq
          eSeq_end = init $ drop k eSeq
          recolored_g = setEdgeColors g (zip eSeq_end cSeq_end)
          interchanged_g = interchangeColors recolored_g hvj c0 cj

{-----====== Helpers ======-----}
otherVertex :: (Eq v) => v -> (v,v) -> v
otherVertex v (v1,v2) = if v == v1 then v2 else v1

colorList :: (Ord v) => Graph v w Integer -> [Integer]
colorList g = [1..highestDegree g + 1]

colorsMissingAt :: (Ord v, Eq c) => Graph v e c -> [c] -> v -> [c]
colorsMissingAt g gColors v = gColors \\ (map edgeData $ incidentEdges g v)

colorsPresentAt :: (Ord v, Eq c) => Graph v e c -> [c] -> v -> [c]
colorsPresentAt g gColors v = intersect gColors (map edgeData $ incidentEdges g v)

kempeSubgraph :: Graph v w Integer -> Integer -> Integer -> Graph v w Integer
kempeSubgraph g c1 c2 = g {edges = filter (\e -> let c = edgeData e in c == c1 || c == c2) $ edges g}

interchangeColors g componentEdgeList c1 c2 = foldl setColor g componentEdgeList
    where setColor gr edge = modifyEdgeUnsafe gr edge edgeFunc
          edgeFunc e = let newColor = if edgeData e == c1 then c2 else c1 in e {edgeData = newColor}

setEdgeColors :: (Eq v, Eq e) => Graph v e c -> [((v,v),c)] -> Graph v e c
setEdgeColors g ls = foldl setEdgeColor g ls

isValidColoring :: (Ord v) => Graph v e Integer -> Bool
isValidColoring g = not . elem False $ map (\v -> noneRepeated $ colorsAt v) (vertices g)
    where colorLs = colorList g
          colorsAt v = foldl (foldF v) [] (edges g)
          foldF v cls e = let (v1,v2) = endpoints e in 
                if v == v1 || v == v2 then (edgeData e):cls
                else cls

noneRepeated :: (Eq a) => [a] -> Bool
noneRepeated ls = ls == nub ls
