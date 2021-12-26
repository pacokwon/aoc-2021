import Data.Bits
import Data.Char (digitToInt)

type Decoded = ([Int], Int)

main :: IO ()
main = interact $ solve . map decodeBinStr . lines

decodeBinStr :: String -> Decoded
decodeBinStr str = (l, foldl1 (\acc cur -> acc * 2 + cur) l)
    where l = map digitToInt str

getCommonAt :: Int -> [Decoded] -> Int
getCommonAt pos codes
    | (uncurry (+) $ quotRem len 2) <= oneCount = 1
    | otherwise = 0
    where oneCount = foldr (+) 0 (map ((!! pos) . fst) codes)
          len = length codes

getUncommonAt :: Int -> [Decoded] -> Int
getUncommonAt pos codes = 1 - getCommonAt pos codes

filterTilUniq :: (Int -> [Decoded] -> Int) -> [Decoded] -> Decoded
filterTilUniq func codes = ftuAux func codes 0

ftuAux :: (Int -> [Decoded] -> Int) -> [Decoded] -> Int -> Decoded
ftuAux func codes@(c:_) pos
  | [x] <- filtered = x
  | digits < pos = error $ show digits ++ " " ++ show pos ++ " " ++ show filtered
  | otherwise = ftuAux func filtered (pos + 1)
  where isStdSet = (func pos codes) > 0
        digits = length . fst $ c
        mask = shiftL 1 (digits - pos - 1)
        filtered = filter ((isStdSet ==) . (> 0) . (.&. mask) . snd) codes

ftuAux _ [] pos = error $ show pos ++ " Empty List!"

solve :: [Decoded] -> String
solve lines = show $ (snd $ filterTilUniq getCommonAt lines) * (snd $ filterTilUniq getUncommonAt lines)
