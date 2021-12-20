import Control.Applicative

main :: IO ()
main = interact $ show . solve . lines

solve :: [String] -> Int
solve (l:ls) = case illegalChar l of
                 Just c -> (codePoint c) + (solve ls)
                 Nothing -> solve ls
solve [] = 0

codePoint :: Char -> Int
codePoint ')' = 3
codePoint ']' = 57
codePoint '}' = 1197
codePoint '>' = 25137
codePoint _   = 0

isClosing :: Char -> Bool
isClosing ')' = True
isClosing ']' = True
isClosing '}' = True
isClosing '>' = True
isClosing _   = False

isPair :: Char -> Char -> Bool
isPair '(' ')' = True
isPair '[' ']' = True
isPair '{' '}' = True
isPair '<' '>' = True
isPair _   _   = False

illegalCharAux :: String -> String -> Maybe Char
illegalCharAux stack (l:rest)
  | isClosing l && (null stack) = Nothing
  | isClosing l = if isPair (head stack) l
                     then illegalCharAux (tail stack) rest
                     else Just l
  | otherwise   = illegalCharAux (l:stack) (rest)
illegalCharAux [] [] = Nothing
illegalCharAux a b = Nothing

illegalChar :: String -> Maybe Char
illegalChar = illegalCharAux []
