--Haskell TicTacToe

import Data.Char
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State.Lazy(StateT, put, get, runStateT)
import Control.Monad.Trans.Reader(ReaderT, runReaderT)
import Control.Monad.IO.Class(liftIO)

data T3Input = X | O | N deriving (Show)

data EndGameLingua = WHATEVER | CONTINUE

instance Eq EndGameLingua where
    WHATEVER == WHATEVER = True
    CONTINUE == CONTINUE = True
    WHATEVER == _ = False
    CONTINUE == _ = False

instance Eq T3Input where
    X == X = True
    O == O = True
    N == N = True
    X == _ = False
    O == _ = False
    N == _ = False

type T3Cell =  (Int, Int, T3Input)

type T3Config = [[(Int, Int)]]

board = [(1,1,N),(2,1,N),(3,1,N),(1,2,N),(2,2,N),(3,2,N),(1,3,N),(2,3,N),(3,3,N)]

victoryindexes = [[(1,1),(2,1),(3,1)],[(1,2),(2,2),(3,2)],[(1,3),(2,3),(3,3)],
                  [(1,1),(1,2),(1,3)],[(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],
                  [(1,1),(2,2),(3,3)],[(1,3),(2,2),(3,1)]]

type TicTacToe a  = StateT [T3Cell] (ReaderT T3Config IO) a

replaceCellInBoard :: T3Cell -> [T3Cell] -> [T3Cell]
replaceCellInBoard (a,b,i) board = map (\(x,y,z) -> if (x==a && y==b && z/=O) then (x,y,i)  else (x,y,z)) board

botPlayMove :: [T3Cell] -> [T3Cell]
botPlayMove board =
    let firstempty@(x,y,z) = head (filter (\(_,_,e) -> e == N) board) in
        replaceCellInBoard (x,y,O) board

isGameOver :: T3Config -> [T3Cell] -> EndGameLingua
isGameOver config board
 | config == [] = CONTINUE
 | (all (\(_,_,e) -> e == X || e == O) board) = WHATEVER
 | otherwise =
 let ([(x1,y1),(x2,y2),(x3,y3)]:zs) = config
     indcheck = (filter (\(a,b,c) -> (x1==a && y1==b) || (x2==a && y2==b) || (x3==a && y3==b)) board) in
         if (all (\(_,_,v) -> v==X) indcheck)
             then WHATEVER
         else isGameOver zs board

convert :: String -> T3Cell
convert (a:b:c:ys) = (digitToInt(a), digitToInt(c), X::T3Input)

runTicTacToe :: TicTacToe ()
runTicTacToe = do
    board <- get
    liftIO $ putStrLn $ show board
    liftIO $ putStrLn "Put an 'X' on the board (Give Entry as: (x-coord, y-coord)"
    entry <- liftIO $ getLine
    put (replaceCellInBoard (convert entry) board)
    usermodified <- get
    case isGameOver victoryindexes usermodified of
         WHATEVER -> put usermodified
         CONTINUE -> do
                     put (botPlayMove usermodified)
                     botmodified <- get
                     case isGameOver victoryindexes botmodified of
                          WHATEVER -> put botmodified
                          CONTINUE -> runTicTacToe

main :: IO()
main = do
    result <- runReaderT (runStateT runTicTacToe board) victoryindexes
    putStrLn (show (snd result))
