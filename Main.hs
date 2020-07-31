import Control.Monad.State
import Data.Maybe

data Mark
  = None | X | O
    deriving (Eq)

instance Show Mark where
  show X    = "X"
  show O    = "O"
  show None = " "

data Board
  = Board [[Mark]]

instance Show Board where
  show (Board [row1, row2, row3])
    =  "\n(Y)\n"
    ++ "   ┏━━━┳━━━┳━━━┓\n"
    ++ " 3 " ++ showRow row3
    ++ "   ┣━━━╋━━━╋━━━┫\n"
    ++ " 2 " ++ showRow row2
    ++ "   ┣━━━╋━━━╋━━━┫\n"
    ++ " 1 " ++ showRow row1
    ++ "   ┗━━━┻━━━┻━━━┛\n"
    ++ "     1   2   3  (X)\n"
      where
        showRow :: [Mark] -> String
        showRow marks
          = "┃" ++ concatMap (\m -> " " ++ (show m ++ " ┃")) marks ++ "\n"

data TicTacToe
  = TicTacToe
    { board :: Board
    , turn  :: Mark
    }

initialState :: TicTacToe
initialState
  = TicTacToe
    { board = Board [[None,None,None],[None,None,None],[None,None,None]]
    , turn  = X
    }

rows :: Board -> [[Mark]]
rows (Board rows')
  = rows'

cols :: Board -> [[Mark]]
cols (Board rows')
  = map col [0,1,2]
    where
      col :: Int -> [Mark]
      col index
        = map (!! index) rows'

diag :: Board -> [[Mark]]
diag (Board rows')
  = [ [rows' !! i !! i | i <- [0,1,2]]
    , [rows' !! i !! (2 - i) | i <- [0,1,2]]
    ]

getWinner :: Board -> Mark
getWinner board
  | playerWon X = X
  | playerWon O = O
  | otherwise   = None
    where
      straights
        = (rows board) ++ (cols board) ++ (diag board)
      playerWon :: Mark -> Bool
      playerWon mark
        = not $ null $ filter (all (== mark)) straights

readMaybe :: Read a => String -> Maybe a
readMaybe
  = listToMaybe . map fst . reads

inBounds :: (Int, Int) -> Bool
inBounds (x, y)
  = x >= 1 && x < 4 && y >= 1 && y < 4

getCell :: Mark -> IO (Int, Int)
getCell mark
  = do
    putStr ("[" ++ (show mark) ++ "] Enter cell: ")
    response <- getLine
    case readMaybe response of
      Just coord@(x,y) | inBounds coord -> return (x - 1, y - 1)
      _                                 -> getCell mark

update :: (a -> a) -> Int -> [a] -> [a]
update func index xs
  = (take index xs) ++ (func (xs !! index) : drop (index + 1) xs)

set :: a -> Int -> [a] -> [a]
set x
  = update (const x)

updateBoard :: (Int, Int) -> Mark -> Board -> Board
updateBoard (x, y) mark (Board rows')
  = Board $ update (set mark x) y rows'

nextTurn :: Mark -> Mark
nextTurn X    = O
nextTurn O    = X
nextTurn None = error "Invalid player None"

playTurn :: StateT TicTacToe IO ()
playTurn
  = do
    currentState <- get
    -- print board
    liftIO $ print $ board currentState
    let currentTurn = turn currentState
    -- get cell
    cell <- liftIO $ getCell currentTurn
    put $ currentState
      { board = updateBoard cell currentTurn (board currentState)
      , turn  = nextTurn currentTurn
      }
    return ()

play :: StateT TicTacToe IO ()
play
  = do
    playTurn
    currentState <- get
    case getWinner $ board currentState of
      None   -> play
      winner -> liftIO $ putStrLn (show winner ++ " has won!")

main :: IO ()
main
  = do
    putStrLn "--- TIC TAC TOE ---"
    putStrLn "Enter coordiantes in the form (x,y) to play"
    evalStateT play initialState
