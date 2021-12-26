{-# LANGUAGE PatternGuards #-}

import Data.List (stripPrefix)
import Control.Monad.State

data Direction = Forward | Down | Up
    deriving (Show, Eq)
type Command = (Direction, Int)

type Horizontal = Int
type Depth = Int
type Aim = Int
type Pos = (Horizontal, Depth, Aim)

main :: IO ()
main = interact $ solve

defaultState :: Pos
defaultState = (0, 0, 0)

dropAim :: Pos -> (Horizontal, Depth)
dropAim (h, d, _) = (h, d)

solve :: String -> String
solve input = show . uncurry (*) . dropAim . fst $ runState (runCommands cmds) defaultState
    where cmds = parseLines . lines $ input

parseLines :: [String] -> [Command]
parseLines = map parseLine
    where parseLine line
            | Just rest <- stripPrefix "forward " line = (Forward, read rest)
            | Just rest <- stripPrefix "down " line    = (Down, read rest)
            | Just rest <- stripPrefix "up " line      = (Up, read rest)
            | otherwise = error "Invalid Line Format!"

runCmdAux :: Command -> (Int, Int, Int) -> ((), Pos)
runCmdAux cmd (horz, depth, aim) = ((), (horz', depth', aim'))
    where (horz', depth', aim') = case cmd of
                     (Forward, x) -> (horz + x, depth + aim * x, aim)
                     (Up, x) -> (horz, depth, aim - x)
                     (Down, x) -> (horz, depth, aim + x)

runCommand :: Command -> State Pos ()
runCommand cmd = state $ runCmdAux cmd

runCommands :: [Command] -> State Pos Pos
runCommands (c:cs) = do
    runCommand c
    runCommands cs
runCommands [] = state $ \s -> (s, s)
