main :: IO ()
main = interact $ show . solve . toIntList . lines

toIntList :: [String] -> [Int]
toIntList = map read

solveAux :: Int -> [Int] -> Int
solveAux acc (x:y:xs)
  | x < y = solveAux (acc + 1) (y:xs)
  | otherwise = solveAux acc (y:xs)
solveAux acc _ = acc

solve :: [Int] -> Int
solve = solveAux 0
