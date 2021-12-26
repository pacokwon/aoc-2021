import Data.List.Split (splitOn)
import Data.List (sortBy, sort, (\\))
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as S

type PLine = ([String], [String])
type Wire = Map.Map Char Char


main :: IO ()
main = interact $ show . sum . map (decode . fillWire . parseLine) . lines

decode :: (Wire, [String]) -> Int
decode (wire, xs) = read . map ((\s -> Map.findWithDefault ' ' s numMap) . sort . decodeAux) $ xs
    where decodeAux = map (\c -> Map.findWithDefault ' ' c wire)

fillWire :: PLine -> (Wire, [String])
fillWire (wires, output) = (revw, output)
    where w    = fillCF wires Map.empty
          w'   = fillEB wires w
          w''  = fillDA wires w'
          w''' = fillG  wires w''
          revw = Map.fromList . map (\(x, y) -> (y, x)) $ Map.toList w'''

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset xs ys = all (`S.member` yset) xs
    where yset = S.fromList ys

batchInsert :: Wire -> [(Char, Char)] -> Wire
batchInsert = foldr (\(k, a) acc -> Map.insert k a acc)

allKeys :: [Char]
allKeys = ['a'..'g']

numMap :: Map.Map String Char
numMap = Map.fromList [ ("abcefg", '0')
                      , ("cf", '1')
                      , ("acdeg", '2')
                      , ("acdfg", '3')
                      , ("bcdf", '4')
                      , ("abdfg", '5')
                      , ("abdefg", '6')
                      , ("acf", '7')
                      , ("abcdefg", '8')
                      , ("abcdfg", '9') ]

fillCF :: [String] -> Wire -> Wire
fillCF xs w = batchInsert w [('c', cWire), ('f', fWire)]
    where zsn = take 3 . drop 6 $ xs
          one = head xs
          six = head . filter (\n -> not $ null (one \\ n)) $ zsn
          cWire = head $ one \\ six
          fWire = head $ one \\ [cWire]

fillEB :: [String] -> Wire -> Wire
fillEB xs w = batchInsert w [('b', bWire), ('e', eWire)]
    where len5 = take 3 . drop 3 $ xs
          cWire = Map.findWithDefault ' ' 'c' w
          fWire = Map.findWithDefault ' ' 'f' w
          five = head . filter (\n -> not (cWire `elem` n)) $ len5
          eWire = head $ allKeys \\ (cWire:five)
          two = head . filter (\n -> eWire `elem` n) $ len5
          bWire = head $ allKeys \\ (fWire:two)

fillDA :: [String] -> Wire -> Wire
fillDA xs w = batchInsert w [('d', dWire), ('a', aWire)]
    where bWire = Map.findWithDefault ' ' 'b' w
          cWire = Map.findWithDefault ' ' 'c' w
          fWire = Map.findWithDefault ' ' 'f' w
          four = xs !! 2
          dWire = head $ four \\ [bWire, cWire, fWire]
          seven = xs !! 1
          aWire = head $ seven \\ [cWire, fWire]

fillG :: [String] -> Wire -> Wire
fillG xs w = Map.insert 'g' gWire w
    where keys = map snd . Map.toList $ w
          gWire = head $ allKeys \\ keys

parseLine :: String -> PLine
parseLine str = let sp = splitOn " | " str
             in (sortBy (compare `on` length) . words . head $ sp, words . head . tail $ sp)

countUniqSegments :: [String] -> Int
countUniqSegments = length . filter (flip elem [2, 3, 4, 7] . length)
