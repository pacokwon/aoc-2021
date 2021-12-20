import Data.List

main :: IO ()
main = interact $ show . takeMiddle . sort . toScores . lines

takeMiddle :: [a] -> a
takeMiddle xs = xs !! (len `div` 2)
    where len = length xs

toScores :: [String] -> [Int]
toScores (l:ls) = case incomplete l of
                 Just s -> (computeScore s 0) : (toScores ls)
                 Nothing -> toScores ls
toScores [] = []

codePoint :: Char -> Int
codePoint ')' = 1
codePoint ']' = 2
codePoint '}' = 3
codePoint '>' = 4
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

getClosing :: Char -> Char
getClosing '(' = ')'
getClosing '{' = '}'
getClosing '[' = ']'
getClosing '<' = '>'

computeScore :: String -> Int -> Int
computeScore (x:xs) acc = computeScore xs acc'
    where acc' = acc * 5 + codePoint x
computeScore [] acc = acc

incompleteAux :: String -> String -> Maybe String
incompleteAux stack (l:rest)
  | isClosing l && (null stack) = Nothing
  | isClosing l = if isPair (head stack) l
                     then incompleteAux (tail stack) rest
                     else Nothing
  | otherwise   = incompleteAux (l:stack) (rest)
incompleteAux stack [] = Just $ map getClosing $ stack

incomplete :: String -> Maybe String
incomplete = incompleteAux []
