module GraphTheory.Graph  where
import GraphTheory.Misc.Infinity
import qualified Data.Map as M
import Text.Printf (printf)
import Data.Maybe (fromJust)

{------===== Data types =====--------}

data Edge v w d = Edge { endpoints :: (v,v),
                         weight    :: w,
                         edgeData  :: d } deriving (Show, Eq)

type BasicEdge v w = Edge v w ()
type ColoredEdge v w c = Edge v w c

data Graph v w d = Graph { vertices :: [v],
                           edges    :: [Edge v w d],
                           directed :: Bool,
                           weighted :: Bool } deriving (Show, Eq)

type BasicGraph v w = Graph v w ()
type ColoredGraph v w c = Graph v w c


uuGraph v w = Graph {vertices = v, edges = w, directed = False, weighted = False}
duGraph v w = Graph {vertices = v, edges = w, directed = True,  weighted = False}
uwGraph v w = Graph {vertices = v, edges = w, directed = False,  weighted = True}
dwGraph v w = Graph {vertices = v, edges = w, directed = True,  weighted = True}


{-----====== Graph building ======-----}

toColored :: BasicGraph v w -> [c] -> ColoredGraph v w c
toColored g cs = g {edges = es'}
    where es' = zipWith zipF (edges g) cs
          zipF edge c = edge {edgeData = c}

-- Input: Graph, list of edge weights.
-- Output: Maybe the graph with modified weights.
assignWeights :: Graph v w c -> [w] -> Maybe (Graph v w c)
assignWeights g weightList@(x:xs)
    | length (edges g) /= length weightList = Nothing
    | otherwise = Just $ g {edges = zipWith (\edge w -> edge {weight = w}) (edges g) weightList}

-- Input: The graph and an edge
-- Output: After checking (in O(|E|) time) to ensure that the edge isn't already present, 
--         return (maybe) the graph with the edge added.

addEdge :: (Eq v, Eq w, Eq c) => Graph v w c -> Edge v w c -> Maybe (Graph v w c)
addEdge g e
    | notElem v1 vs || notElem v2 vs = Nothing
    | directed g = if elem e es 
                   then Just g 
                   else Just $ g {edges = e:es}
    | (not . directed) g = if elem e es || elem (reverseEdge e) es
                           then Just g
                           else Just $ g {edges = e:es}
    where (v1,v2) = endpoints e
          vs = vertices g
          es = edges g

-- Input: The graph and an edge
-- Output: The graph with the edge added in O(1). Duplicate edges could occur
--         as a result of using this function.
addEdgeUnsafe :: (Eq v) => Graph v w c -> Edge v w c -> Graph v w c
addEdgeUnsafe g e = g {edges = e : edges g}


-- O(n): Remove an edge if it exists, and maybe return the graph. TODO:
-- consider making this "unsafe"; just always return a graph.
removeEdge :: (Eq v) => Graph v w c -> (v,v) -> Maybe (Graph v w c)
removeEdge g e
    | null es = Nothing
    | endpoints x == e = Just $ g {edges = xs}
    | (not . directed) g && (v2,v1) == e = Just $ g {edges = xs}
    | otherwise = removeEdge (g {edges=xs}) e >>= 
                        (\h -> return $ h {edges = x : edges h})
    where es = edges g
          (x:xs) = es
          (v1,v2) = endpoints x

{-----====== Edge functions ======-----}

--  Generic edge modification.
--  Input: Graph, edge, function which transforms an edge
--  Output: If the edge exists, modify it with the given function and maybe
--          return the resulting graph.
modifyEdge :: (Eq v, Eq w) => Graph v w c -> (v,v) -> (Edge v w c -> Edge v w c) -> Maybe (Graph v w c)
modifyEdge g toChange f
    | null es = Nothing
    | endpoints x == toChange = Just $ g {edges = f x : xs}
    | (not . directed) g && (v2,v1) == toChange = Just $ g {edges = f x : xs}
    | otherwise = modifyEdge (g {edges=xs}) toChange f >>= 
                            (\g2 -> return $ g2 {edges = x : edges g2})
    where es@(x:xs) = edges g
          vs = vertices g
          (v1,v2) = endpoints x
        
setEdgeWeight :: (Eq v, Eq w) => Graph v w c -> (v,v) -> w -> Maybe (Graph v w c)
setEdgeWeight g e wt' = modifyEdge g e $ setEdgeWeight' wt'
    where setEdgeWeight' w' edge = edge { weight = w' }


weightMatrix :: (Num w, Ord v) => Graph v (Infinitable w) c -> M.Map (v,v) (Infinitable w)
weightMatrix g = foldl (\m e -> M.update (\_ -> Just $ weight e) (endpoints e) m) infMatrix $ allEdges g
    where infMatrix = M.fromList [((v1,v2), if v1 == v2 then Regular 0 else PositiveInfinity) | v1 <- vertices g, v2 <- vertices g]


incidentEdges :: (Eq v) => Graph v w c -> v -> [Edge v w c]
incidentEdges g v = incidentEdges' (edges g) v []

incidentEdges' :: (Eq v) => [Edge v w c] -> v -> [Edge v w c] -> [Edge v w c]
incidentEdges' [] _ incident = incident
incidentEdges' (e:es) v incident
    | (v1 == v) || (v2 == v) = incidentEdges' es v (e:incident)
    | otherwise = incidentEdges' es v incident
    where (v1,v2) = endpoints e

toBasicEdgeList :: [((v,v),w)] -> [BasicEdge v w]
toBasicEdgeList ls = map (\e -> Edge (fst e) (snd e) ()) ls

{-----====== Vertex functions ======-----}

-- Create an adjacency list for a given vertex in O(|E|).
adjacencyList :: (Eq v) => Graph v w c -> v -> [v]
adjacencyList g v = adjacencyList' (edges g) v []

adjacencyList' :: (Eq v) => [Edge v w c] -> v -> [v] -> [v]
adjacencyList' [] _ vls = vls
adjacencyList' (e:es) v vls
    | v1 == v = adjacencyList' es v (v2 : vls)
    | v2 == v = adjacencyList' es v (v1 : vls)
    | otherwise = adjacencyList' es v vls
    where (v1,v2) = endpoints e

reverseEdge :: Edge v w c -> Edge v w c
reverseEdge e@(Edge (v1,v2) _ _) = e { endpoints=(v2,v1) }


reverseEdges :: Graph v w c -> [Edge v w c]
reverseEdges g = map reverseEdge $ edges g


allEdges :: Graph v w c -> [Edge v w c]
allEdges g = if directed g then edges g else edges g ++ reverseEdges g



{-----====== Pretty-printing ======-----}

showWeightMatrix :: (Num w, Ord v, Show v, Show w) => Graph v (Infinitable w) c -> IO ()
showWeightMatrix g = do
    putStr "   "
    mapM_ (printf "%-3v" . show) (vertices g)
    mapM_ (\x -> putStr "\n" >> printf "%-3v" (show x) >> printMatrixRow g x ) (vertices g)
    putStr "\n"

printMatrixRow :: (Num w, Ord v, Show v, Show w) => Graph v (Infinitable w) c -> v -> IO ()
printMatrixRow g v = mapM_ (printf "%-3v" . show . w v) (vertices g)
    where wts = weightMatrix g
          w u1 u2 = fromJust $ M.lookup (u1,u2) wts
