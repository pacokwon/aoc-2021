import Data.List.Split (splitOn)

type Range = (Int, Int)
type Area = (Range, Range)

main :: IO ()
main = interact $ show . parse

-- ex> target area: x=20..30, y=-10..-5
--  -> [target area: x], [20..30, y], [-10..-5]
parse :: String -> Area
parse str = (xRange, yRange)
    where (_:f:s:_) = splitOn "=" str
          parseDots :: String -> Range
          parseDots dotStr = let rng = splitOn ".." dotStr
                              in (read . head $ rng, read . head . tail $ rng)
          xRange = parseDots $ take (length f - 3) f
          yRange = parseDots $ s

computeXVelRange :: Range -> [Int]
computeXVelRange (l, h) = takeWhile ((<= h) . natSum) . dropWhile ((< l) . natSum) $ [1..]
    where natSum n = (n * (n + 1)) `div` 2
