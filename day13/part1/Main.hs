import Data.List.Split (splitWhen, splitOn)
import Data.List (stripPrefix, nub, sort)

type Pos = (Int, Int)
type Fold = (Char, Int)

main :: IO ()
main = interact $ showFoldedPos . (uncurry afterSingleFold) . parse . lines

showFoldedPos :: [Pos] -> String
showFoldedPos poss = unlines $ [[ if (j, i) `elem` poss then '#' else '.' | j <- [0..width]] | i <- [0..height]]
    where height = foldr1 max (map snd poss)
          width  = foldr1 max (map fst poss)

afterSingleFold :: [Fold] -> [Pos] -> [Pos]
afterSingleFold (f:fs) = nub . map (foldAlong f)

foldAlong :: Fold -> Pos -> Pos
foldAlong ('x', l) p@(x, y)
  | l < x = (2 * l - x, y)
  | otherwise = p
foldAlong ('y', l) p@(x, y)
  | l < y = (x, 2 * l - y)
  | otherwise = p
foldAlong _ _ = error "Invalid Fold!"

parsePos :: String -> Pos
parsePos str = let split = splitOn "," str
                in (read . head $ split, read . head . tail $ split)

parseFold :: String -> Fold
parseFold str = let (Just rest) = stripPrefix "fold along " str
                    split       = splitOn "=" rest
                 in (head . head $ split, read . head . tail $ split)

parse :: [String] -> ([Fold], [Pos])
parse ls = (folds, poss)
    where split = splitWhen null ls
          poss = map parsePos $ head split
          folds = map parseFold $ head . tail $ split
