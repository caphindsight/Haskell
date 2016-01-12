module Graph where

import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector as V

newtype IdN = IdN Int deriving (Show, Eq)
newtype IdE = IdE Int deriving (Show, Eq)

data Node n = Node {
    incomingEdges :: [IdE],
    outgoingEdges :: [IdE],
    nodeData      :: n
} deriving (Show)

data Edge e = Edge {
    source   :: IdN,
    target   :: IdN,
    edgeData :: e
} deriving (Show)

data Graph n e = Graph {
    nodes :: V.Vector (Node n),
    edges :: V.Vector (Edge e)
}

nodesCount g = V.length $ nodes g
nodeIds g = map IdN [0..(n-1)] where n = nodesCount g

edgesCount g = V.length $ edges g
edgeIds g = map IdE [0..(n-1)] where n = edgesCount g

node g (IdN id)
  | id >= 0 && id < n  = nodes g V.! id
  | otherwise          = error $ "Graph node id #" ++ show id ++ " is out of range 0.." ++ show (n-1) ++ "!"
  where n = nodesCount g

edge g (IdE id)
  | id >= 0 && id < n  = edges g V.! id
  | otherwise          = error $ "Graph edge id #" ++ show id ++ " is out of range 0.." ++ show (n-1) ++ "!"
  where n = edgesCount g

mkGraph :: [n] -> [(Int, Int, e)] -> Graph n e
mkGraph ns es =
    let nsi = zip [0..] ns
        esi = zip [0..] es
    in Graph {
        nodes = V.fromList $ map (\(i, n) -> Node {
            incomingEdges = map (IdE . fst) $ filter (\(_, (_, t, _)) -> t == i) esi,
            outgoingEdges = map (IdE . fst) $ filter (\(_, (s, _, _)) -> s == i) esi,
            nodeData = n
        }) nsi,
        edges = V.fromList $ map (\(s, t, d) -> Edge {
            source = IdN s,
            target = IdN t,
            edgeData = d
        }) es
    }

data DFSEventType = DFSEnter | DFSLeave deriving (Show, Eq)
data DFSEvent = DFSEvent IdN DFSEventType deriving (Show)

dfs :: Graph n e -> IdN -> [DFSEvent]
dfs g n = runST $ do
    visited <- fmap V.fromList $ sequence $ replicate (nodesCount g) $ newSTRef False
    dfsST g n visited
  where
    dfsST g n@(IdN idn) v = do
        dead <- readSTRef (v V.! idn)
        if dead
            then return []
            else do
                writeSTRef (v V.! idn) True
                childs <- sequence $ map (\e -> dfsST g (target $ edge g e) v) $ (outgoingEdges $ node g n)
                let flattened = foldl (++) [] childs
                return $ [DFSEvent n DFSEnter] ++ flattened ++ [DFSEvent n DFSLeave]

