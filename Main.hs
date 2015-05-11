module Main where

import GraphTheory.Graph
import GraphTheory.SpecialGraphs
import qualified Data.Map as M

infinity = 1/0

g1 :: Graph Integer Integer
g1 = uwGraph [1, 2, 3, 4, 5, 6] [((1,2),0), ((3,2),0),((5,3),6),((4,1),10),((5,6),1)]

--dijkstra :: Graph v e -> [(v,e)]
--dijkstra g = dijkstra' g (Hash.new (==) (Hash.hashString . show))

main = putStrLn $ show g1
