module SpecialGraphs (complete) where
import Graph (Graph(..), Edge(..))

complete :: Integer -> Graph Integer Integer 
complete n = Graph [1..n] [((x,y),1) | x <- [1..n], y <- [x..n], x /= y]
