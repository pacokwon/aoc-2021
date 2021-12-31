import Control.Monad.State
import Data.List.Split (splitOn)

data Expr = Var Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Eql Expr Expr

data AluState = AluState
  { varCount :: Int
  , x :: Expr
  , y :: Expr
  , z :: Expr
  , w :: Expr
  }

main :: IO ()
main =

parseLine :: String -> Expr

readInput :: IO ()
readInput = readFile "input"
  <&> lines
  <&>
  <&> parseLine
