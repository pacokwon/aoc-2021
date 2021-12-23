-- Brute Force Method

import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List (group, sort, sortBy)
import Data.Function (on)

type Template = String
type RuleBook = Map.Map String Char

main :: IO ()
main = interact $ show . solve . (uncurry applyInsertion) . parse . lines

solve :: Template -> Int
solve = (uncurry (-)) . getFirstAndLast . sortBy (flip (compare `on` snd)) . map (\x -> (head x, length x)) . group . sort
    where getFirstAndLast xs = (snd . head $ xs, snd . last $ xs)

applyInsertion :: RuleBook -> Template -> Template
applyInsertion rb tpl = iterate (insertPair rb) tpl !! 10

parse :: [String] -> (RuleBook, Template)
parse (t:_:rb) = (parseRuleBook rb, t)
parse _ = error "Wrong Input Format!"

parseRuleBook :: [String] -> RuleBook
parseRuleBook strs = Map.fromList $ map parseLine strs
    where parseLine l = let split = splitOn " -> " l
                         in (head split, head . head . tail $ split)

splitTemplate :: Template -> [String]
splitTemplate xs = aux xs []
    where aux []  acc = reverse acc
          aux [x] acc = reverse acc
          aux xs  acc = aux (drop 1 xs) $ (take 2 xs) : acc

blend :: String -> [Maybe Char] -> String
blend (x:xs) ((Just c):ys) = x:c:(blend xs ys)
blend (x:xs) (Nothing:ys)  = x:(blend xs ys)
blend xs     []            = xs

insertPair :: RuleBook -> Template -> Template
insertPair rb tpl = inserted
    where insertAux :: String -> Maybe Char -> String
          insertAux (p1:p2:[]) (Just c)  = [p1, c, p2]
          insertAux (p1:p2:[]) Nothing = [p1, p2]
          insertAux _ _ = error "Wront Pair Format!"

          pairs      = splitTemplate tpl
          insertions = map (flip Map.lookup rb) pairs
          inserted   = blend tpl insertions

