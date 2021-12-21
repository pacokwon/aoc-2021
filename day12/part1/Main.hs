import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List
import Data.Function (on)
import Data.Char (isUpper)
import Control.Monad.State
import Control.Monad.State

type Adj = Map.Map String [String]
type VisitMap = Map.Map String Bool
type PathState = (Adj, VisitMap)

inp = makeAdj . makeDuplex .splitLines . lines $ "start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end"

startCave :: String
startCave = "start"

endCave :: String
endCave = "end"

main :: IO ()
main = interact $ show . length . solve . makeAdj . makeDuplex . splitLines . lines

-- the map's value indicates whether the cave has already been visited or not
-- if the value is True, then the cave has already been visited
solve :: Adj -> [[String]]
solve adj = computePaths startCave [] vmap adj
    where vmap = Map.fromList [(startCave, True)] -- mark start as visited

isBigCave :: String -> Bool
isBigCave = isUpper . head

makeAdj :: [(String, String)] -> Adj
makeAdj = Map.fromList . map tfGroup . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    where tfGroup edges@((from, to):_) = (from, map snd edges)

makeDuplex :: [(String, String)] -> [(String, String)]
makeDuplex = concatMap (\(from, to) -> [(from, to), (to, from)])

splitLines :: [String] -> [(String, String)]
splitLines = map splitLine
    where splitLine :: String -> (String, String)
          splitLine line = let (f:s:_) = splitOn "-" line
                            in (f, s)

computePaths :: String -> [String] -> VisitMap -> Adj -> [[String]]
computePaths start path vmap adj | start == endCave = [(start:path)]
computePaths start path vmap adj =
    let (Just dests) = Map.lookup start adj
        visitable    = filter (\dest -> (isBigCave dest) || (not $ Map.findWithDefault False dest vmap)) dests
     in concatMap (\v -> computePaths v (start:path) (Map.alter (const $ Just True) v vmap) adj) visitable
