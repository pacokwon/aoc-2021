import Data.List.Split (splitOn)

main :: IO ()
main = interact $ show . snd . solve . parse

solve :: [Int] -> (Int, Int)
solve xs = binSearchDest xs minPos maxPos
    where maxPos = foldr1 max xs
          minPos = foldr1 min xs

inp :: [Int]
inp = [16,1,2,0,4,2,7,1,2,14]

parse :: String -> [Int]
parse = map read . splitOn ","

calcCost :: [Int] -> Int -> Int
calcCost poss dest = sum . map (abs . (dest -)) $ poss

binSearchDest :: [Int] -> Int -> Int -> (Int, Int)
binSearchDest poss left right
    | left == right = (left, lc)
    | (lc < mc && mc < rc) || (lc < rc) = binSearchDest poss left (mid - 1)
    | (rc < mc && mc < lc) || (rc < lc) = binSearchDest poss (mid + 1) right
    | otherwise = (mid, mc)
    where mid = (left + right) `div` 2
          mc = calcCost poss mid
          lc = calcCost poss left
          rc = calcCost poss right
