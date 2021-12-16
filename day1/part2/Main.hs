main :: IO ()
main = interact $ show . solve . toIntList . lines

toIntList :: [String] -> [Int]
toIntList = map read

solveAux :: Int -> [Int] -> Int
solveAux acc l@(x1:x2:x3:x4:xs)
  | (x1 + x2 + x3) < (x2 + x3 + x4) = solveAux (acc + 1) (tail l)
  | otherwise = solveAux acc (tail l)
solveAux acc _ = acc

solve :: [Int] -> Int
solve = solveAux 0
