import Data.List.Split (splitOn)
import Data.List (transpose, findIndex)
import Data.Maybe
import Text.Printf (printf)

type Input = Int
type Inputs = [Input]
type Board = [[(Int, Bool)]]
data BoardPos = Row Int | Col Int

inp = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
\\n\
\22 13 17 11  0\n\
\8  2 23  4 24\n\
\21  9 14 16  7\n\
\6 10  3 18  5\n\
\1 12 20 15 19\n\
\\n\
\3 15  0  2 22\n\
\9 18 13 17  5\n\
\19  8  7 25 23\n\
\20 11 10 24  4\n\
\14 21 16 12  6\n\
\\n\
\14 21 17 24  4\n\
\10 16 15  9 19\n\
\18  8 23 26 20\n\
\22 11 13  6  5\n\
\2  0 12  3  7"

main :: IO ()
main = interact $ show . calculateAnswer . firstWinningBoard . parseLines . filter (not . null) . lines

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

getBingoBoard :: [Board] -> Maybe (Board, BoardPos)
getBingoBoard [] = Nothing
getBingoBoard (b:bs) = case getBingoPos b of
                         Just pos -> Just (b, pos)
                         Nothing -> getBingoBoard bs

markBoards :: Int -> [Board] -> [Board]
markBoards target = map (markBoard target)

markBoard :: Int -> Board -> Board
markBoard target = map (map $ \(num, marked) -> (num, marked || (target == num)))

firstWinningBoard :: (Inputs, [Board]) -> (Input, Board)
firstWinningBoard ([], boards) = error $ (unlines $ map showBoard boards) ++ "Not supposed to happen"
firstWinningBoard (input:rest, boards) =
    let boards' = markBoards input boards
     in case getBingoBoard boards' of
          Just (board, pos) -> (input, board)
          Nothing -> firstWinningBoard (rest, boards')

calculateAnswer :: (Input, Board) -> Int
calculateAnswer (input, board) = sumOfUnmarked * input
    where sumOfUnmarked = sum . map fst . filter ((== False). snd) . mconcat $ board
