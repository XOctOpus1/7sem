import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

-- Функція активації (сигмоїда)
sigmoid :: Matrix R -> Matrix R
sigmoid x = 1 / (1 + exp (-x))

-- Ініціалізація мережі
initializeNetwork :: Int -> Int -> Int -> Matrix R
initializeNetwork inputSize hiddenSize outputSize = vcat [rand inputSize hiddenSize, rand hiddenSize outputSize]

-- Прохід сигналу через мережу
feedForward :: Matrix R -> Matrix R -> Matrix R
feedForward network input = sigmoid (input <> network)

-- Навчання мережі методом зворотного поширення помилки
backpropagation :: Matrix R -> Matrix R -> Matrix R -> Double -> Int -> Matrix R
backpropagation network input target learningRate numIterations =
  iterate numIterations network
  where
    iterate 0 w = w
    iterate n w =
      let
        -- Проходження вперед
        a = input
        z = feedForward w a

        -- Обчислення похибки
        δ = z - target

        -- Зворотнє поширення помилки
        delta = δ * z * (1 - z)
        gradient = a `outer` delta

        -- Оновлення ваг мережі
        w' = w - (learningRate * gradient)
      in
        iterate (n - 1) w'

main :: IO ()
main = do
  let inputSize = 2
      hiddenSize = 4
      outputSize = 1
      learningRate = 0.1
      numIterations = 1000

  let input = (3><2) [1.0, 0.0, 0.0, 1.0, 1.0, 1.0]
      target = (3><1) [1.0, 0.0, 1.0]

  let network = initializeNetwork inputSize hiddenSize outputSize
      trainedNetwork = backpropagation network input target learningRate numIterations

  putStrLn "Trained Network:"
  print trainedNetwork