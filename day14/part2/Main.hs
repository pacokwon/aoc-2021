{-# LANGUAGE TupleSections #-}

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (group, groupBy, sort, sortBy)
import Data.Function (on)

type Template = String
type Pair = (Char, Char)
type RuleBook = Map.Map Pair Char
type Counter  = Map.Map Pair Int

main :: IO ()
main = interact $ show . (uncurry solve) . parse . lines

computeFreq :: Counter -> Char -> Char -> [(Char, Int)]
computeFreq cntr first last = sortBy (flip $ on compare snd) realStats
    --    lst :: [((Char, Char), Int)]
    where lst = Map.toList cntr
    --    scattered :: [(Char, Int)]
          scattered = concatMap (\((x, y), c) -> [(x, c), (y, c)]) lst
    --    grouped :: [[(Char, Int)]]
          grouped = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ scattered
    --    stats :: [(Char, Int)]
          stats = map (\row -> (fst . head $ row, sum . map snd $ row)) grouped
          realStats = map (\(c, s) -> if c == first || c == last then (c, (s - 1) `div` 2 + 1) else (c, s `div` 2)) stats

solve :: RuleBook -> Template -> Int
solve rb tpl = (snd . head $ freq) - (snd . last $ freq)
    where cntr = (iterate (nextCount rb) (makeCounter tpl)) !! 40
          f = head tpl
          l = last tpl
          freq = computeFreq cntr f l

parse :: [String] -> (RuleBook, Template)
parse (t:_:rb) = (parseRuleBook rb, t)
parse _ = error "Wrong Input Format!"

parseRuleBook :: [String] -> RuleBook
parseRuleBook strs = Map.fromList $ map parseLine strs
    where parseLine l = let split = splitOn " -> " l
                         in ((head . head $ split, head . tail . head $ split), head . head . tail $ split)

splitTemplate :: Template -> [(Char, Char)]
splitTemplate xs = aux xs []
    where aux []  acc = reverse acc
          aux [x] acc = reverse acc
          aux xs  acc = aux (drop 1 xs) $ ((\x -> (head x, head . tail $ x)) . take 2 $ xs) : acc

makeCounter :: Template -> Counter
makeCounter = foldr (Map.alter aux) Map.empty . splitTemplate
    where aux :: Maybe Int -> Maybe Int
          aux (Just v) = Just $ v + 1
          aux Nothing  = Just 1

insertFunc :: RuleBook -> (Pair, Int) -> Counter -> Counter
insertFunc rb (p@(f, s), cnt) = Map.alter aux (m, s) . Map.alter aux (f, m)
    where (Just m) = Map.lookup p rb
          aux :: Maybe Int -> Maybe Int
          aux (Just v) = Just $ v + cnt
          aux Nothing  = Just cnt

nextCount :: RuleBook -> Counter -> Counter
nextCount rb cntr = foldr (insertFunc rb) Map.empty (Map.toList cntr)
