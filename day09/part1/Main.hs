{-# LANGUAGE BlockArguments #-}

import Data.Array
import Data.Char (digitToInt)

type Pos = (Int, Int)
type Height = Int
type Floor = Array Pos Height

main :: IO ()
main = interact $ show . sumRiskLevels . findLowPoints . parse . lines

sumRiskLevels :: [(Pos, Height)] -> Int
sumRiskLevels = sum . map ((+1) . snd)

findLowPoints :: Floor -> [(Pos, Height)]
findLowPoints floor = map (\p -> (p, floor ! p)) . filter isLower $ range b
    where b@(_, (height, width)) = bounds floor
          dv = zip [0, 1, 0, -1] [-1, 0, 1, 0]
          outOfRange (x, y) = not $ 0 <= x && x <= height && 0 <= y && y <= width
          isLower (x, y) = all (== True) do (dx, dy) <- dv
                                            let x' = x + dx
                                            let y' = y + dy
                                            return $ outOfRange (x', y') || (floor ! (x, y) < floor ! (x', y'))


parse :: [String] -> Floor
parse l@(x:xs) = listArray bounds (map digitToInt . concat $ l)
    where h = length l - 1
          w = length x - 1
          bounds = ((0, 0), (h, w))
