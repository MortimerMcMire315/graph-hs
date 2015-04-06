module Graph where

type Edge v e = ((v,v),e)
data Graph v e = Graph {vertices :: [v], edges :: [Edge v e] } deriving (Show, Eq)

g1 :: Graph Integer Integer
g1 = Graph [1, 2, 3, 4, 5] [((1,2),0), ((3,2),0)]

assignWeights :: Graph v e -> [e] -> Maybe (Graph v e)
assignWeights (Graph verts edges) weightList@(x:xs)
    | length edges /= length weightList = Nothing
    | otherwise = Just (Graph verts (zipWith (\x y -> (fst x, y)) edges weightList))

addEdge :: (Eq v) => Graph v e -> Edge v e -> Maybe (Graph v e)
addEdge (Graph vs es) e@((v1,v2),_)
    | not (elem v1 vs) || not (elem v2 vs) = Nothing
    | otherwise = Just (Graph vs (e:es))

setEdgeWeight :: Edge v e -> e -> Edge v e
setEdgeWeight (vs,w) w' = (vs,w')

setEdgeWeightG :: (Eq v, Eq e) => Graph v e -> (v,v) -> e -> Maybe (Graph v e)
setEdgeWeightG (Graph vs es@(x:xs)) toChange w'
    | null es = Nothing
    | fst x == toChange = Just $ Graph vs $ setEdgeWeight x w':xs
    | otherwise = setEdgeWeightG (Graph vs xs) toChange w'

main = putStrLn $ show g1
