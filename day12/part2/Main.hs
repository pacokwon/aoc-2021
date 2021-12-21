import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Data.List
import Data.Function (on)
import Data.Char (isUpper)
import Control.Monad.State
import Control.Monad.State

type Adj = Map.Map String [String]
type VisitMap = Map.Map String Int
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

visitLimit :: Int
visitLimit = 2

main :: IO ()
main = interact $ show . length . solve . makeAdj . makeDuplex . splitLines . lines

-- the map's value indicates how many times the cave has been visited
-- if the value is 1, then the cave has been visited once
solve :: Adj -> [[String]]
solve adj = computePaths startCave [] vmap adj
    where vmap = Map.fromList [(startCave, 2), (endCave, 1)] -- startCave cannot be visited again, endCave can only be visited once

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

isSmallVisitedTwice :: VisitMap -> Bool
isSmallVisitedTwice vmap = Map.foldrWithKey aux False vmap
    where aux :: String -> Int -> Bool -> Bool
          aux key val acc = acc || (foldr1 (&&) [key /= startCave, key /= endCave, not $ isBigCave key, val >= 2])

computePaths :: String -> [String] -> VisitMap -> Adj -> [[String]]
computePaths start path vmap adj | start == endCave = [(start:path)]
computePaths start path vmap adj =
    let (Just dests) = Map.lookup start adj
        -- if big cave, visitable
        -- if small cave, visitable if :
        --      it was visited once and no other small cave was visited twice
        --   OR it was not visited before
        filterFunc :: String -> Bool
        filterFunc cave = (isBigCave cave) || (cave == "end") || (vcount == 1 && (not $ isSmallVisitedTwice vmap)) || (vcount == 0)
            where vcount = Map.findWithDefault 0 cave vmap

        visitable    = filter filterFunc dests
        alterFunc :: Maybe Int -> Maybe Int
        alterFunc (Just count) = Just $ count + 1
        alterFunc Nothing     = Just 1
     in concatMap (\v -> computePaths v (start:path) (Map.alter alterFunc v vmap) adj) visitable
