module Main where

import Graph.Graph

g1 :: Graph Integer Integer
g1 = Graph [1, 2, 3, 4, 5] [((1,2),0), ((3,2),0),((5,3),6)]

main = putStrLn $ show g1
