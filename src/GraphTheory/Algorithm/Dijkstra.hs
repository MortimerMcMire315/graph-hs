module GraphTheory.Algorithm.Dijkstra where

import Data.Maybe (fromJust)
import Data.List ((\\))
import qualified Data.Map as M
import GraphTheory.Misc.Infinity (Infinitable (..))
import GraphTheory.Graph (weightMatrix, vertices, Graph)

showDijkstra g u = do
    let dijkstraResult = dijkstra g u
    mapM_ (\x -> putStr "\n" >> putStr (show (fst x) ++ ": " ++ show (snd x))) dijkstraResult
    putStr "\n"
        
dijkstra :: (Ord v, Ord w, Num w, Show v, Show w) => Graph v (Infinitable w) c -> v -> [(v, Infinitable w)]
dijkstra g u = dijkstra' g wt l [u]
    where l = M.update (\_ -> Just $ Regular 0) u $ M.fromList $ map vertWeightPair vs
          vertWeightPair vert = (vert, fromJust $ M.lookup (u,vert) wt)
          wt = weightMatrix g
          vs = vertices g


dijkstra' :: (Ord v, Ord w, Num w, Show v, Show w) => Graph v w c -> M.Map (v,v) w -> M.Map v w -> [v] -> [(v,w)]
dijkstra' g wt lMap t
    | length t == length vs = M.toList newL
    | otherwise = dijkstra' g wt newL (v':t)
    where vs = vertices g
          comparev' tuple1 tuple2 = if snd tuple1 < snd tuple2 then tuple1 else tuple2
          newL = foldl updateL lMap vNotInT
          updateL lMap' vert = if l' vert > (l' v' + w (v',vert))
                               then M.update (\_ -> Just $ l' v' + w (v',vert)) vert lMap'
                               else lMap'
                where l' vert = fromJust $ M.lookup vert lMap'
          v' = fst $ foldl1 comparev' $ map (\v -> (v,l v)) vNotInT
          l vert = fromJust $ M.lookup vert lMap
          w edge = fromJust $ M.lookup edge wt
          vNotInT = vs \\ t
