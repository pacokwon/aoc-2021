import Data.List.Split (splitOn)
import Data.List (transpose, findIndex)
import Data.Maybe
import Text.Printf (printf)

type Input = Int
type Inputs = [Input]
type Board = [[(Int, Bool)]]
data BoardPos = Row Int | Col Int

main :: IO ()
main = interact $ show . calculateAnswer . lastWinningBoard . parseLines . filter (not . null) . lines

showBoards :: [Board] -> String
showBoards = unlines . map showBoard

showBoard :: Board -> String
showBoard = unlines . map (unwords . rowToString)
    where rowToString = map (\(num, marked) -> let padded = printf "%2d" num in if marked then "[" ++ padded ++ "]" else " " ++ padded ++ " " )

parseBoard :: [String] -> [Board]
parseBoard [] = []
parseBoard xs = (map (map (\x -> (read x, False)) . words) (take 5 xs)) : (parseBoard $ drop 5 xs)

parseLines :: [String] -> (Inputs, [Board])
parseLines (first:rest) = (input, boards)
    where input = map read . splitOn "," $ first
          boards = parseBoard rest

getBingoPos :: Board -> Maybe BoardPos
getBingoPos board = case checkHorBingo board of
                       Just row -> Just $ Row row
                       Nothing ->
                           case checkVerBingo board of
                             Just col -> Just $ Col col
                             Nothing -> Nothing
    where checkHorBingo = findIndex (== True) . map (all (== True) . map snd)
          checkVerBingo = checkHorBingo . transpose

getBingoBoards :: [Board] -> Maybe [(Board, Int)]
getBingoBoards = gbbAux 0
    where gbbAux _ [] = Nothing
          gbbAux pos (b:bs) = case getBingoPos b of
                                Just _ -> case gbbAux (pos + 1) bs of
                                    Just res -> Just $ (b, pos):res
                                    Nothing -> Just [(b, pos)]
                                Nothing -> gbbAux (pos + 1) bs

markBoards :: Int -> [Board] -> [Board]
markBoards target = map (markBoard target)

markBoard :: Int -> Board -> Board
markBoard target = map (map $ \(num, marked) -> (num, marked || (target == num)))

lastWinningBoard :: (Inputs, [Board]) -> (Input, Board)
lastWinningBoard ([], boards) = error $ '\n' : (unlines $ map showBoard boards) ++ "Not supposed to happen"
lastWinningBoard (input:rest, boards) =
    let boards' = markBoards input boards
        len     = length boards'
        bboards  = getBingoBoards boards'
     in case bboards of
          Just bboards' ->
              let ps = map snd $ bboards' in
              if len == 1
                 then (input, head boards')
                 else lastWinningBoard (rest, [b | (b, i) <- zip boards' [0..], not $ i `elem` ps])
          Nothing -> lastWinningBoard (rest, boards')

calculateAnswer :: (Input, Board) -> Int
calculateAnswer (input, board) = sumOfUnmarked * input
    where sumOfUnmarked = sum . map fst . filter ((== False). snd) . mconcat $ board
