import Data.List

type Graph a = [(a, [a])]

findAllPaths :: Eq a => a -> a -> Graph a -> [[a]]
findAllPaths start end graph
    | start == end = [[end]]
    | otherwise = [start:path | neighbor <- neighbors, path <- findAllPaths neighbor end (removeNode neighbor graph)]
    where neighbors = maybe [] id (lookup start graph)
          removeNode node = filter ((/= node) . fst)

main :: IO ()
main = do
    let graph :: Graph Int
        graph = [(1, [2, 3]), (2, [3, 4]), (3, [4]), (4, [1])]
    let startVertex = 1
    let endVertex = 4
    let paths = findAllPaths startVertex endVertex graph
    print paths
