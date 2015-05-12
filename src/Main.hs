module Main where

import GraphTheory.Graph (showWeightMatrix, Graph (..), dwGraph)
import GraphTheory.Misc.Infinity (Infinitable (..))
import GraphTheory.Algorithm.Dijkstra (dijkstra, showDijkstra)

g1 :: Graph Integer (Infinitable Integer)
g1 = dwGraph [0, 1, 2, 3, 4] [((0,1),1),((0,2),3),((0,4),6),((1,2),1),((1,3),3),((2,0),1),((2,3),1),((2,1),2),((3,0),3),((3,4),2),((4,3),1)]

main = do
    putStrLn $ "g = " ++ (show g1)
    let u = head $ vertices g1
    putStrLn "\nWeight matrix:"
    showWeightMatrix g1
    putStr "\n"
    putStr "Dijkstra algorithm for u=0:"
    showDijkstra g1 u
