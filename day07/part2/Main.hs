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

sigma :: Int -> Int
sigma n = (n * (n + 1)) `div` 2

calcCost :: [Int] -> Int -> Int
calcCost poss dest = sum . map (sigma . abs . (dest -)) $ poss

binSearchDest :: [Int] -> Int -> Int -> (Int, Int)
binSearchDest poss left right
    | left == right = (left, lc)
    | (left + 1 == right) && (lc < rc) = (left, lc)
    | (left + 1 == right) = (right, rc)
    | (lc < mc && mc < rc) || (lc < rc) = binSearchDest poss left mid
    | (rc < mc && mc < lc) || (rc < lc) = binSearchDest poss mid right
    | otherwise = (mid, mc)
    where mid = (left + right) `div` 2
          mc = calcCost poss mid
          lc = calcCost poss left
          rc = calcCost poss right
