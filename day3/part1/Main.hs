main :: IO ()
main = interact $ solve . lines

dissectCode :: String -> [Int]
dissectCode (x:xs)
  | x == '0'  = 0:(dissectCode xs)
  | otherwise = 1:(dissectCode xs)
dissectCode [] = []

-- accept list of codes and return how many 1's are in each digit place
countOnes :: [String] -> [Int]
countOnes = foldr1 (zipWith (+)) . map dissectCode

binStrToInt :: String -> Int
binStrToInt ('0':xs) = 0 + 2 * binStrToInt xs
binStrToInt ('1':xs) = 1 + 2 * binStrToInt xs
binStrToInt []     = 0
binStrToInt otherwise = error "Invalid Input!"

oneStatToBinary :: Int -> [Int] -> Int
oneStatToBinary _ [] = 0
oneStatToBinary len xs = binStrToInt $ reverse [if x > (len `div` 2) then '1' else '0' | x <- xs]

solve :: [String] -> String
solve lines@(l:_) = show $ gamma * epsilon
    where len = length lines
          gamma = oneStatToBinary len . countOnes $ lines
          epsilon = 2 ^ (length l) - 1 - gamma
