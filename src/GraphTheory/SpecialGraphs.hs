module GraphTheory.SpecialGraphs (complete) where
import GraphTheory.Graph (Graph(..), Edge(..), uuGraph)

complete :: Integer -> Graph Integer Integer Integer
complete n = uuGraph [1..n] [Edge (x,y) 1 1 | x <- [1..n], y <- [x..n], x /= y]
