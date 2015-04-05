module Graph where

type WeightedEdge v e = ((v,v),e)
data WeightedGraph v e = WeightedGraph {vertices :: [v], edges :: [WeightedEdge v e] } deriving (Show, Eq)

g1 :: WeightedGraph Integer Integer
g1 = WeightedGraph [1, 2, 3, 4, 5] [((1,2),0), ((3,2),0)]

assignWeights :: WeightedGraph v e -> [e] -> Maybe (WeightedGraph v e)
assignWeights (WeightedGraph verts edges) weightList@(x:xs)
    | length edges /= length weightList = Nothing
    | otherwise = Just (WeightedGraph verts (zipWith (\x y -> (fst x, y)) edges weightList))

addWeightedEdge :: (Eq v) => WeightedGraph v e -> WeightedEdge v e -> Maybe (WeightedGraph v e)
addWeightedEdge (WeightedGraph vs es) e@((v1,v2),_)
    | not (elem v1 vs) || not (elem v2 vs) = Nothing
    | otherwise = Just (WeightedGraph vs (e:es))

main = putStrLn $ show g1
