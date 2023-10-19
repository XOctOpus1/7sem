import Data.List
import Data.Maybe

type Position = (Int, Int)
type Board = [[Int]]

-- Рухи коня
moves :: [Position]
moves = [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]

-- Перевірка, чи клітинка знаходиться на дошці та не була вже відвідана
isValidMove :: Board -> Position -> Position -> Bool
isValidMove board (x, y) (dx, dy) =
  let n = length board
  in x + dx >= 0 && x + dx < n && y + dy >= 0 && y + dy < n && board !! (x + dx) !! (y + dy) == 0

-- Знаходження шляху коня за допомогою DFS
findKnightTour :: Int -> Position -> Maybe [Position]
findKnightTour n start = go (replicate n (replicate n 0)) start [start]
  where
    go board cur path
      | length path == n * n = Just path
      | otherwise =
        let nextMoves = filter (isValidMove board cur) moves
        in listToMaybe . catMaybes $ map (\move -> go (updateBoard board move) (addPos cur move) (cur : path)) nextMoves

    addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    updateBoard board move = 
      let (x, y) = addPos cur move
      in replace2D x y 1 board

replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D i j val rows = take i rows ++ [replace1D j val (rows !! i)] ++ drop (i + 1) rows

replace1D :: Int -> a -> [a] -> [a]
replace1D i val xs = take i xs ++ [val] ++ drop (i + 1) xs

main :: IO ()
main = do
  let n = 8  -- Розмір шахівниці
  let start = (0, 0)  -- Початкова позиція коня
  let solution = findKnightTour n start
  case solution of
    Just path -> mapM_ print path
    Nothing -> putStrLn "Рішення не знайдено"
