module Grasph.SpecialGraphs (complete, completeBipartite) where
import Grasph.Graph (Graph(..), ColoredGraph, Edge(..), uuGraph)
import Grasph.Misc.Infinity

complete :: Integer -> ColoredGraph Integer InfInt Integer
complete n = uuGraph [1..n] [Edge (x,y) 1 1 | x <- [1..n], y <- [x..n], x /= y]

completeBipartite :: Integer -> Integer -> ColoredGraph Integer InfInt Integer
completeBipartite x y = uuGraph [1..(x + y)] [Edge (a,b) 1 1 | a <- [1..x], b <- [(x+1)..(x+y)], x /= y]
