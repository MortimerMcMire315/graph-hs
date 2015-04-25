module Graph where

type Edge v e = ((v,v),e)
data Graph v e = Graph {vertices :: [v], edges :: [Edge v e] } deriving (Show, Eq)

g1 :: Graph Integer Integer
g1 = Graph [1, 2, 3, 4, 5] [((1,2),0), ((3,2),0),((5,3),6)]

assignWeights :: Graph v e -> [e] -> Maybe (Graph v e)
assignWeights (Graph verts edges) weightList@(x:xs)
    | length edges /= length weightList = Nothing
    | otherwise = Just (Graph verts (zipWith (\x y -> (fst x, y)) edges weightList))

addEdge :: (Eq v) => Graph v e -> Edge v e -> Maybe (Graph v e)
addEdge (Graph vs es) e@((v1,v2),_)
    | not (elem v1 vs) || not (elem v2 vs) = Nothing
    | otherwise = Just (Graph vs (e:es))

removeEdge :: (Eq v) => Graph v e -> (v,v) -> Maybe (Graph v e)
removeEdge (Graph vs []) _ = Nothing
removeEdge (Graph vs es@(x:xs)) e@(v,w)
    | null es = Nothing
    | fst x == e = Just $ Graph vs xs
    | otherwise = case removeEdge (Graph vs xs) e of
                           Nothing -> Nothing
                           Just (Graph vs es) -> Just (Graph vs (x:es))

setEdgeWeightG :: (Eq v, Eq e) => Graph v e -> (v,v) -> e -> Maybe (Graph v e)
setEdgeWeightG g e wt' = modifyEdgeG g e (\x -> setEdgeWeight x wt')
    where setEdgeWeight (vs,w) w' = (vs,w')

modifyEdgeG :: (Eq v, Eq e) => Graph v e -> (v,v) -> (Edge v e -> Edge v e) -> Maybe (Graph v e)
modifyEdgeG (Graph vs []) _ _ = Nothing
modifyEdgeG (Graph vs es@(x:xs)) toChange f
    | null es = Nothing
    | fst x == toChange = Just $ Graph vs $ (f x):xs
    | otherwise = case modifyEdgeG (Graph vs xs) toChange f of
                        Nothing -> Nothing
                        Just (Graph vs es) -> Just (Graph vs (x:es))

adjacencyList :: (Eq v) => Graph v e -> v -> [v]
adjacencyList g v = adjacencyList' (edges g) v []

adjacencyList' :: (Eq v) => [Edge v e] -> v -> [v] -> [v]
adjacencyList' [] _ vls = vls
adjacencyList' (e:es) v vls
    | firstVertex e == v = adjacencyList' es v (secondVertex e : vls)
    | secondVertex e == v = adjacencyList' es v (firstVertex e : vls)
    | otherwise = adjacencyList' es v vls
    where secondVertex edge = (snd . fst) edge
          firstVertex edge = (fst . fst) edge

main = putStrLn $ show g1
