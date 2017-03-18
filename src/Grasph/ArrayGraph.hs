module Grasph.ArrayGraph where

import Data.Array ( Array
                  , array
                  , (//)
                  , (!)
                  )

data Edge w d = Edge { weight    :: w
                     , edgeData  :: d
                     } deriving (Show, Eq)

newtype ArrayGraph w d = ArrayGraph { adjList :: Array (Int, Int) (Maybe (Edge w d))
                                    } deriving (Show, Eq)

graphWithNoEdges :: Int -> ArrayGraph w d
graphWithNoEdges n
    | n < 1 = error "Graph must have at least one vertex."
    | otherwise = ArrayGraph $ array ((1,1),(n,n)) [((x,y),Nothing) | x <- [1..n], y <- [1..n]]

addEdge :: ArrayGraph w d -> (Int,Int) -> Edge w d -> ArrayGraph w d
addEdge g (v1,v2) e = g { adjList = newAdjList }
    where newAdjList = adjList g // [((v1,v2), Just e)]

getEdge :: ArrayGraph w d -> (Int,Int) -> Maybe (Edge w d)
getEdge g = (!) (adjList g)
