module Main where

import GraphTheory.Graph
import GraphTheory.SpecialGraphs
import GraphTheory.Misc.Infinity (Infinitable (..), InfInt)
import GraphTheory.Algorithm.Dijkstra (dijkstra, showDijkstra)
import GraphTheory.Algorithm.DFS
--import GraphTheory.Algorithm.EdgeColor


g1 :: BasicGraph Integer InfInt
g1 = dwGraph [0, 1, 2, 3, 4] $ toBasicEdgeList [((0,1),1),((0,2),3),((0,4),6),((1,2),1),((1,3),3),((2,0),1),((2,3),1),((2,1),2),((3,0),3),((3,4),2),((4,3),1)]

g2 = dwGraph [0, 1, 2, 3] $ toBasicEdgeList [((0,1),3),((0,2),2),((0,1),4),((2,3),1)]
g3 = toColored g1 [1,6,5,7,6,5,4,3,2,1,4]

makeColorGraph :: [v] -> [(v,v)] -> ColoredGraph v Integer Integer
makeColorGraph vs es = Graph vs coloredEdgeList False False
    where coloredEdgeList = zipWith zipF es $ take (length es) [1,1..]
          zipF (v1,v2) c = Edge (v1,v2) 1 c

k5 = complete 5

g4 :: BasicGraph Integer InfInt
g4 = uuGraph [0, 1, 2, 3, 4,5,6,7,8,9,10] $ toBasicEdgeList [((0,1),1),((0,2),1),((1,2),1),((3,4),1),
                                                             ((5,6),1),((0,6),1),((8,9),1),((9,10),1)]

main = do
    putStrLn $ "g = " ++ show g1
    let u = head $ vertices g1
    putStrLn "\nWeight matrix:"
    showWeightMatrix g1
    putStr "\n"
    putStr "Dijkstra algorithm for u=0:"
    showDijkstra g1 u
