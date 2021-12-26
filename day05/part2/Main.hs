{-# LANGUAGE NamedFieldPuns #-}

import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Pos = (Int, Int)
type Line = (Pos, Pos)
type Floor = Map.Map Pos Int

main :: IO ()
main = interact solve

solve :: String -> String
solve inp = show . countPoints (>= 2) $ markLines inputLines floor
    where inputLines = parseLines . lines $ inp
          floor = Map.empty

-- "x1,y1 -> x2,y2" -> ((x1, y1), (x2, y2))
parseLine :: String -> Line
parseLine str = let (from:_:to:_) = words str
                    (x1:y1:_) = map read . splitOn "," $ from :: [Int]
                    (x2:y2:_) = map read . splitOn "," $ to :: [Int]
                 in ((x1, y1), (x2, y2))

parseLines :: [String] -> [Line]
parseLines = map parseLine

-- generate a list of positions from a line
-- ex> ((5, 5), (5, 9)) returns [(5, 5), (5, 6), (5, 7), (5, 8), (5, 9)]
getPosRange :: Line -> [Pos]

-- vertical line
getPosRange ((x, y1), (x', y2)) | x == x' =
    let ly = max y1 y2
        sy = min y1 y2
     in [(x, y) | y <- [sy..ly]]

-- horizontal line
getPosRange ((x1, y), (x2, y')) | y == y' =
    let lx = max x1 x2
        sx = min x1 x2
     in [(x, y) | x <- [sx..lx]]

-- major diagonal line
getPosRange (p1@(x1, y1), p2@(x2, y2))
  | (x1 + y1 == x2 + y2) = if x1 < x2
                              then zip [x1..x2] [y1,y1-1..y2]
                              else getPosRange (p2, p1)
  | (x1 - y1 == x2 - y2) = if x1 < x2
                              then zip [x1..x2] [y1..y2]
                              else getPosRange (p2, p1)

getPosRange _ = []

markPos :: Pos -> Floor -> Floor
markPos = Map.alter f
    where f (Just v) = Just $ v + 1
          f Nothing  = Just 1

markLine :: Line -> Floor -> Floor
markLine line floor =
    let poss = getPosRange line
     in foldr markPos floor poss

markLines :: [Line] -> Floor -> Floor
markLines lines floor = foldr markLine floor lines

countPoints :: (Int -> Bool) -> Floor -> Int
countPoints predicate floor = Map.foldr f 0 floor
        where f cur acc = acc + (if predicate cur then 1 else 0)

-- Map.alter f 7 (Map.fromList [(5,0), (3,0)]) == Map.fromList [(3,0), (5,0)]
