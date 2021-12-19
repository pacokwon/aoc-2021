{-# LANGUAGE BlockArguments #-}

import Data.Array
import Data.Char (digitToInt)
import Data.List

import qualified System.IO
import qualified GHC.IO.Handle

type Pos = (Int, Int)
type Height = Int
type Floor = Array Pos Height

dvs :: [(Int, Int)]
dvs = zip [0, 1, 0, -1] [-1, 0, 1, 0]

-- filename = "input"
-- https://stackoverflow.com/questions/50931355/how-can-i-test-a-program-reading-from-stdin-in-ghci
-- main :: IO ()
-- main = do
--     h <- System.IO.openFile filename System.IO.ReadMode
--     oldStdin <- GHC.IO.Handle.hDuplicate System.IO.stdin
--     GHC.IO.Handle.hDuplicateTo h System.IO.stdin
--     System.IO.hClose h
--     realMain
--     GHC.IO.Handle.hDuplicateTo oldStdin System.IO.stdin

main :: IO ()
main = interact $ show . solve . parse . lines

solve :: Floor -> Int
solve floor = foldr1 (*) . take 3 . sortBy (flip compare) . map (length . basinSpots floor) $ lps
    where lps = findLowPoints floor

basinSpots :: Floor -> Pos -> [Pos]
basinSpots f p = nub $ basinSpotsAux f p

basinSpotsAux :: Floor -> Pos -> [Pos]
basinSpotsAux floor (x, y) = (x, y) : (concat . map (basinSpotsAux floor) $ candidates)
    where b@(_, (height, width)) = bounds floor
          outOfRange (x, y) = not $ 0 <= x && x <= height && 0 <= y && y <= width
          candidates :: [Pos]
          candidates = do let v = floor ! (x, y)
                          (dx, dy) <- dvs
                          let x' = x + dx
                          let y' = y + dy
                          let v' = floor ! (x', y')
                          if (not $ outOfRange (x', y')) && (v < v') && (v' /= 9)
                             then return (x', y')
                             else []


findLowPoints :: Floor -> [Pos]
findLowPoints floor = filter isLower $ range b
    where b@(_, (height, width)) = bounds floor
          outOfRange (x, y) = not $ 0 <= x && x <= height && 0 <= y && y <= width
          isLower (x, y) = all (== True) do (dx, dy) <- dvs
                                            let x' = x + dx
                                            let y' = y + dy
                                            return $ outOfRange (x', y') || (floor ! (x, y) < floor ! (x', y'))


parse :: [String] -> Floor
parse l@(x:xs) = listArray bounds (map digitToInt . concat $ l)
    where h = length l - 1
          w = length x - 1
          bounds = ((0, 0), (h, w))
