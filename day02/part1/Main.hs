{-# LANGUAGE PatternGuards #-}

import Data.List (stripPrefix)
import Control.Monad.State

data Direction = Forward | Down | Up
    deriving (Show, Eq)
type Command = (Direction, Int)
type Pos = (Int, Int)

main :: IO ()
main = interact $ solve

solve :: String -> String
solve input = show . uncurry (*) . fst $ runState (runCommands cmds) (0, 0)
    where cmds = parseLines . lines $ input

parseLines :: [String] -> [Command]
parseLines = map parseLine
    where parseLine line
            | Just rest <- stripPrefix "forward " line = (Forward, read rest)
            | Just rest <- stripPrefix "down " line    = (Down, read rest)
            | Just rest <- stripPrefix "up " line      = (Up, read rest)
            | otherwise = error "Invalid Line Format!"

runCmdAux :: Command -> (Int, Int) -> ((), Pos)
runCmdAux cmd (horz, depth) = ((), (horz', depth'))
    where (horz', depth') = case cmd of
                     (Forward, x) -> (horz + x, depth)
                     (Up, x) -> (horz, depth - x)
                     (Down, x) -> (horz, depth + x)

runCommand :: Command -> State Pos ()
runCommand cmd = state $ runCmdAux cmd

runCommands :: [Command] -> State Pos Pos
runCommands (c:cs) = do
    runCommand c
    runCommands cs
runCommands [] = state $ \s -> (s, s)
