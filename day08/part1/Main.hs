import Data.List.Split (splitOn)

type PLine = ([String], [String])

main :: IO ()
-- main = interact $ show . map countUniqSegments . map snd . map parseLine . lines
main = interact $ show . sum . map (countUniqSegments . snd . parseLine) . lines

parseLine :: String -> PLine
parseLine str = let sp = splitOn " | " str
             in (words . head $ sp, words . head . tail $ sp)

countUniqSegments :: [String] -> Int
countUniqSegments = length . filter (flip elem [2, 3, 4, 7] . length)
