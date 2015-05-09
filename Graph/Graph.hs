module Graph.Graph (Graph(..),Edge,
             assignWeights,addEdge,removeEdge,setEdgeWeight,modifyEdge,
             adjacencyList) where

{------===== Data types =====--------}
type Edge v e = ((v,v),e)
data Graph v e = Graph {vertices :: [v], edges :: [Edge v e] } deriving (Show, Eq)


{-----====== Graph building ======-----}
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

{-----====== Edge functions ======-----}

--  Generic edge modification.
--  Input: Graph, edge, function which transforms an edge
--  Output: Maybe a new graph.
modifyEdge :: (Eq v, Eq e) => Graph v e -> (v,v) -> (Edge v e -> Edge v e) -> Maybe (Graph v e)
modifyEdge (Graph vs []) _ _ = Nothing
modifyEdge (Graph vs es@(x:xs)) toChange f
    | null es = Nothing
    | fst x == toChange = Just $ Graph vs $ (f x):xs
    | otherwise = case modifyEdge (Graph vs xs) toChange f of
                        Nothing -> Nothing
                        Just (Graph vs es) -> Just (Graph vs (x:es))

setEdgeWeight :: (Eq v, Eq e) => Graph v e -> (v,v) -> e -> Maybe (Graph v e)
setEdgeWeight g e wt' = modifyEdge g e (\x -> setEdgeWeight' x wt')
    where setEdgeWeight' (vs,w) w' = (vs,w')



{-----====== Vertex functions ======-----}
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
