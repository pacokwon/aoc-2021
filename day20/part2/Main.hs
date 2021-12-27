{-# LANGUAGE TupleSections #-}

-- Credits to https://github.com/414owen/advent-of-code-2021/blob/master/src/Ad20.hs !
-- I solved the problem by referring to the solution above

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

type Point = (Int, Int)

main :: IO ()
main = interact $ show . S.size . uncurry solve . parse . filter (not . null) . lines

parse :: [String] -> (Vector Bool, Set Point)
parse (rule:image) = (rule', points)
  where
    rule' = V.fromList . map (== '#') $ rule
    points = S.fromList [(i, j) | (i, row) <- zip [0..] image, (j, cell) <- zip [0..] row, cell == '#']

getBound :: Set Point -> ((Int, Int), (Int, Int))
getBound image = ((minimum xs - 1, maximum xs + 1), (minimum ys - 1, maximum ys + 1))
  where (xs, ys) = unzip $ S.toList image

getSquare :: Point -> [Point]
getSquare (x, y) = do
  x' <- [x-1..x+1]
  y' <- [y-1..y+1]
  return (x', y')

bin2dec :: [Int] -> Int
bin2dec = foldl' (\acc cur -> acc * 2 + cur) 0

step :: Int -> Vector Bool -> Set Point -> (Int, Set Point)
step nth rule image = (nth+1,) $ S.fromList $ do
    x <- [lx..hx]
    y <- [ly..hy]
    if getNextPixel (x, y)
       then return (x, y)
       else []
  where
    ((lx, hx), (ly, hy)) = getBound image

    isLit :: Point -> Bool
    isLit p@(x, y) = (nth `mod` 2 == 1 && (x <= lx || x >= hx || y <= ly || y >= hy)) || S.member p image

    square2bin :: [Point] -> [Int]
    square2bin = map (\b -> if b then 1 else 0) . map isLit

    square2dec :: [Point] -> Int
    square2dec = bin2dec . square2bin

    getNextPixel :: Point -> Bool
    getNextPixel = (rule !) . square2dec . getSquare

iterstep :: Vector Bool -> Set Point -> [Set Point]
iterstep rule image = snd <$> iterate aux (0, image)
  where aux :: (Int, Set Point) -> (Int, Set Point)
        aux (nth, img) = step nth rule img

solve :: Vector Bool -> Set Point -> Set Point
solve rule image = iterstep rule image !! 50
