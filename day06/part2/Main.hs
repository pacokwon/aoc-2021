import Data.List.Split (splitOn)
import Data.Array
import Data.Ix

birthCycle :: Int
birthCycle = 7

totalDays :: Int
totalDays = 256

main :: IO ()
main = interact $ show . solve . parse

parse :: String -> [Int]
parse = map read . splitOn ","

-- https://stackoverflow.com/questions/49764615/converting-java-like-2d-dynamic-programing-matrix-to-haskell
-- ^ about dynamic programming in haskell. helped a lot!
memoGrowth :: Int -> Int -> Int
memoGrowth timer days = arr ! (timer, days)
    where
        bounds = ((0, 0), (birthCycle + 1, totalDays))

        arr = listArray bounds (map (uncurry growth) (range bounds))

        growth _ 0 = 1
        growth t d
          | t == 0    = arr ! (birthCycle - 1, d - 1) + arr ! (birthCycle + 1, d - 1)
          | otherwise = arr ! (t - 1, d - 1)

solve :: [Int] -> Int
solve = sum . map (flip memoGrowth totalDays)
