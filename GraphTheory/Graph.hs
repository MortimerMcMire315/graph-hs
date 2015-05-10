module GraphTheory.Graph {--(Graph(..),Edge,
             assignWeights,addEdge,removeEdge,setEdgeWeight,modifyEdge,
             adjacencyList)--} where

{------===== Data types =====--------}
type Edge v e = ((v,v),e)
data Graph v e = Graph {vertices :: [v], edges :: [Edge v e], directed :: Bool, weighted :: Bool} deriving (Show,Eq)

uuGraph v e = Graph {vertices = v, edges = e, directed = False, weighted = False}
duGraph v e = Graph {vertices = v, edges = e, directed = True,  weighted = False}
uwGraph v e = Graph {vertices = v, edges = e, directed = False,  weighted = True}
dwGraph v e = Graph {vertices = v, edges = e, directed = True,  weighted = True}


{-----====== Graph building ======-----}
assignWeights ::  Graph v e -> [e] -> Maybe (Graph v e)
assignWeights g weightList@(x:xs)
    | length e /= length weightList = Nothing
    | otherwise = Just $ g {edges = zipWith (\x y -> (fst x, y)) e weightList}
    where v = vertices g
          e = edges g

addEdge :: (Eq v) => Graph v e -> Edge v e -> Maybe (Graph v e)
addEdge g e@((v1,v2),_)
    | not (elem v1 vs) || not (elem v2 vs) = Nothing
    | directed g = if (elem (v1,v2) (map fst es)) 
                   then Just g 
                   else Just $ g {edges = (e:es)}
    | (not . directed) g = if (elem (v1,v2) (map fst es)) || (elem (v2,v1) (map fst es)) 
                           then Just g
                           else Just $ g {edges = (e:es)}
    where vs = vertices g
          es = edges g

addEdgeUnsafe :: (Eq v) => Graph v e -> Edge v e -> Graph v e
addEdgeUnsafe g e = g {edges = e:(edges g)}

removeEdge :: (Eq v) => Graph v e -> (v,v) -> Maybe (Graph v e)
removeEdge g e
    | null es = Nothing
    | fst x == e = Just $ g {edges = xs}
    | (not . directed) g && (v2,v1) == e = Just $ g {edges = xs}
    | otherwise = removeEdge (g {edges=xs}) e >>= 
                        (\h -> return $ h {edges = x:(edges h)})
    where es = edges g
          (x:xs) = es
          (v1,v2) = fst x

{-----====== Edge functions ======-----}

--  Generic edge modification.
--  Input: Graph, edge, function which transforms an edge
--  Output: Maybe a new graph.
modifyEdge :: (Eq v, Eq e) => Graph v e -> (v,v) -> (Edge v e -> Edge v e) -> Maybe (Graph v e)
modifyEdge g toChange f
    | null es = Nothing
    | fst x == toChange = Just $ g {edges = (f x):xs}
    | (not . directed) g && (v2,v1) == toChange = Just $ g {edges = (f x):xs}
    | otherwise = modifyEdge (g {edges=xs}) toChange f >>= 
                            (\g2 -> return $ g2 {edges = x:(edges g2)})
    where es = edges g
          vs = vertices g
          (x:xs) = es
          (v1,v2) = fst x
        
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
--}
