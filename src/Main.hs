module Main where

import GraphTheory.Graph
import GraphTheory.SpecialGraphs
import GraphTheory.Misc.Infinity (Infinitable (..), InfInt)
import GraphTheory.Algorithm.Dijkstra (dijkstra, showDijkstra)
import GraphTheory.Algorithm.DFS
import GraphTheory.Algorithm.EdgeColor
import System.Random (getStdGen, next)
import Control.Monad (foldM)


g1 :: BasicGraph Integer InfInt
g1 = dwGraph [0, 1, 2, 3, 4] $ toBasicEdgeList [((0,1),1),((0,2),3),((0,4),6),((1,2),1),((1,3),3),((2,0),1),((2,3),1),((2,1),2),((3,0),3),((3,4),2),((4,3),1)]

g2 = dwGraph [0, 1, 2, 3] $ toBasicEdgeList [((0,1),3),((0,2),2),((0,1),4),((2,3),1)]
g3 = toColored g1 [1,6,5,7,6,5,4,3,2,1,4]

k5 = complete 5

g4 :: BasicGraph Integer InfInt
g4 = uuGraph [0, 1, 2, 3, 4,5,6,7,8,9,10] $ toBasicEdgeList [((0,1),1),((0,2),1),((1,2),1),((3,4),1),
                                                             ((5,6),1),((0,6),1),((8,9),1),((9,10),1)]

--g5 :: Graph Integer Integer Integer
--g5 = makeColorGraph [1,2,3,4,5,6,7,8,9,10] [(2,3),(6,8) ,(7,9) ,(4,10),(4,5) ,(7,8) ,(7,3) ,(9,4) ,(9,1) ,(8,6) ,(8,3) ,(10,8),(2,1) ,(9,2) ,(1,5) ,(3,5) ,(3,1) ,(8,5) ,(10,5),(8,10),(7,6) ,(4,8) ,(1,9) ,(5,2) ,(1,3) ,(6,4) ,(4,3) ,(7,4) ,(5,9) ,(4,9) ,(3,9) ,(6,3) ,(4,1) ,(2,7) ,(7,5) ,(4,6) ,(1,10),(2,6) ,(3,10),(2,10)]



--main = do
--    let d = deltaPlusOneColor g5
--    putStrLn $ show d
--    putStrLn . show $ isValidColoring d

main = do
    g <- getStdGen
    foldM foldFunc g [1..100] 

foldFunc gen _ = do
    let (_, newg) = next gen 
    let r = randomGraph gen 30 240
    let str = if isValidColoring $ deltaPlusOneColor r
              then (show . head $ edges r) ++ " " ++ show True
              else show False ++ "\n" ++ (show r)
    putStrLn str
    return newg
