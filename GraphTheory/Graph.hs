module GraphTheory.Graph  where
import Misc.Infinity
import qualified Data.Map as M
import Text.Printf (printf)
import Data.Maybe (fromJust)

{------===== Data types =====--------}
type Edge v e = ((v,v),e)
data Graph v e = Graph {vertices :: [v], edges :: [Edge v e], directed :: Bool, weighted :: Bool} deriving (Show,Eq)

uuGraph v e = Graph {vertices = v, edges = e, directed = False, weighted = False}
duGraph v e = Graph {vertices = v, edges = e, directed = True,  weighted = False}
uwGraph v e = Graph {vertices = v, edges = e, directed = False,  weighted = True}
dwGraph v e = Graph {vertices = v, edges = e, directed = True,  weighted = True}


{-----====== Graph building ======-----}

-- Input: Graph, list of edge weights.
-- Output: Maybe the graph with modified weights.
assignWeights ::  Graph v e -> [e] -> Maybe (Graph v e)
assignWeights g weightList@(x:xs)
    | length e /= length weightList = Nothing
    | otherwise = Just $ g {edges = zipWith (\x y -> (fst x, y)) e weightList}
    where v = vertices g
          e = edges g

-- Input: The graph and an edge
-- Output: After checking (in O(n) time) to ensure that the edge isn't already present, 
--         return (maybe) the graph with the edge added.
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

-- Input: The graph and an edge
-- Output: The graph with the edge added in O(1). Duplicate edges could occur
--         as a result of using this function.
addEdgeUnsafe :: (Eq v) => Graph v e -> Edge v e -> Graph v e
addEdgeUnsafe g e = g {edges = e:(edges g)}


-- O(n): Remove an edge if it exists, and maybe return the graph. TODO:
-- consider making this "unsafe"; just always return a graph.
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
--  Output: If the edge exists, modify it with the given function and maybe
--          return the resulting graph.
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


weightMatrix :: (Num e,Ord v) => Graph v (Infinitable e) -> M.Map (v,v) (Infinitable e)
weightMatrix g = foldl (\m e -> M.update (\_ -> Just $ snd e) (fst e) m) infMatrix $ allEdges g
    where infMatrix = M.fromList [((v1,v2), if v1 == v2 then Regular 0 else PositiveInfinity) | v1 <- vertices g, v2 <- vertices g]


{-----====== Vertex functions ======-----}

-- Create an adjacency list for a given vertex in O(|E|).
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

reverseEdge :: ((a,a),b) -> ((a,a),b)
reverseEdge ((v1,v2),x) = ((v2,v1),x)

reverseEdges :: Graph v e -> [((v,v),e)]
reverseEdges g = map reverseEdge $ edges g

allEdges :: Graph v e -> [((v,v),e)]
allEdges g = if (directed g) then edges g else (edges g) ++ (reverseEdges g)


{-----====== Pretty-printing ======-----}

showWeightMatrix :: (Num e,Ord v,Show v, Show e) => Graph v (Infinitable e) -> IO ()
showWeightMatrix g = do
    putStr "   "
    mapM_ (\x -> printf "%-3v" $ show x) (vertices g)
    mapM_ (\x -> do putStr "\n" >> (printf "%-3v" (show x)) >> printMatrixRow g x ) (vertices g)
    putStr "\n"

printMatrixRow :: (Num e, Ord v, Show v, Show e) => Graph v (Infinitable e) -> v -> IO ()
printMatrixRow g v = mapM_ (\v2 -> printf "%-3v" $ show $ w v v2) (vertices g)
    where wts = weightMatrix g
          w u1 u2 = fromJust $ M.lookup (u1,u2) wts
