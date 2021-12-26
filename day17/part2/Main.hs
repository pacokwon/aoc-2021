import Data.List.Split (splitOn)

type Range = (Int, Int)
type Area = (Range, Range)
type State = (Int, Int, Int, Int)
data Status = Valid | Invalid | Continue deriving (Show, Eq)

main :: IO ()
main = interact $ show . length . getPossible . parse
-- main = interact $ show . getBounds . parse
-- main = interact $ const $ show $ singleStep $ singleStep (0, 0, 6, (-10))

getPossible :: Area -> [(Int, Int)]
getPossible area = filter (simulate area) . getBounds $ area

getBounds :: Area -> [(Int, Int)]
getBounds (xb, yb) = do
  vx <- xvelBound xb
  vy <- yvelBound yb
  return (vx, vy)

-- ex> target area: x=20..30, y=-10..-5
--  -> [target area: x], [20..30, y], [-10..-5]
parse :: String -> Area
parse str = (xRange, yRange)
  where
    (_:f:s:_) = splitOn "=" str
    parseDots :: String -> Range
    parseDots dotStr = let rng = splitOn ".." dotStr
                        in (read . head $ rng, read . head . tail $ rng)
    xRange = parseDots $ take (length f - 3) f
    yRange = parseDots $ s

-- x velocity:
-- lower bound: inverse natural number
-- upper bound: the larger x in the given input
xvelBound :: Range -> [Int]
xvelBound (l, h) = [lower..higher]
  where natSum n = (n * (n + 1)) `div` 2
        lower = head . dropWhile ((< l) . natSum) $ [1..]
        higher = h

-- y velocity:
-- lower bound: inverse natural number
-- upper bound: the larger y in the given input
yvelBound :: Range -> [Int]
yvelBound (l, h) = [lower..higher]
  where higher = (-1) - l
        lower = l

judge :: Area -> State -> Status
judge ((lx, hx), (ly, hy)) (x, y, _, _)
  | inside    = Valid
  | overY     = Invalid
  | otherwise = Continue
  where inside = lx <= x && x <= hx && ly <= y && y <= hy
        overY = y < ly

singleStep :: State -> State
singleStep (x, y, vx, vy) = (x + vx, y + vy, vx', vy')
  where vx' = if vx > 0 then vx - 1 else if vx == 0 then vx else vx + 1
        vy' = vy - 1

stepUntilEnd :: Area -> State -> Status
stepUntilEnd area s = let res = singleStep s
                        in case judge area res of
                             Valid -> Valid
                             Invalid -> Invalid
                             Continue -> stepUntilEnd area res

simulate :: Area -> (Int, Int) -> Bool
simulate area (vx, vy) = case stepUntilEnd area (0, 0, vx, vy) of
                            Valid -> True
                            Invalid -> False
                            Continue -> error "Shouldn't come out"
