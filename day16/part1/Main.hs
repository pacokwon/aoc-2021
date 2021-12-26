-- module Test where
module Main where

import Parser
import Control.Applicative ((<|>))
import Control.Monad
import Data.Char (digitToInt)
import Data.List (foldl')

type Version = Int
type TypeId = Int
data Packet = Literal Version TypeId Int | Operator Version TypeId [Packet]
    deriving (Show)

main :: IO ()
main = interact $ solve . head . lines

solve :: String -> String
solve inp = let (Right p) = runParser packet . hex2bin $ inp
         in show . addVersionNos . result $ p

addVersionNos :: Packet -> Int
addVersionNos (Literal v _ _) = v
addVersionNos (Operator v _ ps) = v + (sum . map addVersionNos $ ps)

bit :: Parser Char
bit = char '0' <|> char '1'

bits :: Int -> Parser String
bits = (flip iterM) bit

iterM :: Int -> Parser a -> Parser [a]
iterM n p = sequence . take n . repeat $ p

version :: Parser Int
version = bin2dec <$> (sequence . take 3 . repeat $ bit)

typeId :: Parser Int
typeId = bin2dec <$> (sequence . take 3 . repeat $ bit)

groupsAux :: Parser [String]
groupsAux = do
    bs <- bits 5

    if (head bs) == '0'
       then return [tail bs]
       else do rest <- groupsAux
               return $ (tail bs):rest

groups :: Parser Int
groups = (bin2dec . concat) <$> groupsAux

packet :: Parser Packet
packet = literalValue <|> operatorValue

literalValue :: Parser Packet
literalValue = do
    v <- version
    tid <- (bin2dec <$> str "100")
    gs <- groups
    return $ Literal v tid gs

operatorValue :: Parser Packet
operatorValue = do
    v <- version
    tid <- typeId
    subps <- subPackets
    return $ Operator v tid subps

subPackets :: Parser [Packet]
subPackets = do
    b <- bit
    if b == '0'
       then do len <- (bin2dec <$> bits 15)
               res <- subpLength len
               return res
       else do count <- (bin2dec <$> bits 11)
               res <- subpCount count
               return res

subpCount :: Int -> Parser [Packet]
subpCount count = do
    p <- packet
    if count == 1
       then return [p]
       else do rest <- subpCount (count - 1)
               return (p:rest)


subpLength :: Int -> Parser [Packet]
subpLength totalLen = do
    (res, len) <- withLength packet
    if len == totalLen
       then return [res]
       else do rest <- subpLength (totalLen - len)
               return (res:rest)

bin2dec :: String -> Int
bin2dec = foldl' (\acc c -> acc * 2 + digitToInt c) 0

hex2bin :: String -> String
hex2bin = concatMap aux
    where aux :: Char -> String
          aux '0' = "0000"
          aux '1' = "0001"
          aux '2' = "0010"
          aux '3' = "0011"
          aux '4' = "0100"
          aux '5' = "0101"
          aux '6' = "0110"
          aux '7' = "0111"
          aux '8' = "1000"
          aux '9' = "1001"
          aux 'A' = "1010"
          aux 'B' = "1011"
          aux 'C' = "1100"
          aux 'D' = "1101"
          aux 'E' = "1110"
          aux 'F' = "1111"
