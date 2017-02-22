--Haskell TicTacToe

import Data.Char
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State.Lazy(StateT, put, get, runStateT)
import Control.Monad.Trans.Reader(ReaderT, runReaderT)
import Control.Monad.IO.Class(liftIO)

data T3Input = X | O | N deriving (Show)

instance Eq T3Input where
    X == X = True
    O == O = True
    N == N = True

type T3Cell =  (Int, Int, T3Input)

type T3Config = [[(Int, Int)]]

board = [(1,1,N),(2,1,N),(3,1,N),(1,2,N),(2,2,N),(3,2,N),(1,3,N),(2,3,N),(1,3,N)]


victoryindexes = [[(1,1),(2,1),(3,1)],[(1,2),(2,2),(3,2)],[(1,3),(2,3),(3,3)],
                  [(1,1),(1,2),(1,3)],[(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],
                  [(1,1),(2,2),(3,3)],[(1,3),(2,2),(3,1)]]


type TicTacToe a  = StateT [T3Cell] (ReaderT T3Config IO) a

replaceCellInBoard :: T3Cell -> [T3Cell] -> [T3Cell]
replaceCellInBoard (a,b,i) board =
    map (\(x,y,z) -> if (x==a && y==b) then (x,y,i)  else (x,y,z)) board
			
runTicTacToe :: TicTacToe ()
runTicTacToe = do
                 liftIO $ putStrLn "Put an 'X' on the board (Give Coordinates as X,Y)"
                 --theboard <- get
--                 cell@(x,y,i) <- getLine
--                 case i of
--                      'X'   -> put (replaceCell cell theboard)
--                      'O'   -> 

main :: IO()
main = do result <- runReaderT (runStateT runTicTacToe board) victoryindexes
          putStrLn (show (snd result))



{-

checkVictory :: [T3Cell] -> String
checkVictory board@((a,b,c):ys)
 | length board == 0                                                            = "Keep playing!"
 | all (\(_,_,e) -> e == X || e == O) board                                     = "Noboby Wins, Too bad."
 | ((a==1 && b==1 && c==X) && (a==2 && b==1 && c==X) && (a==3 && b==1 && c==X)) = "You Won!" 
 | ((a==1 && b==2 && c==X) && (a==2 && b==2 && c==X) && (a==3 && b==2 && c==X)) = "You Won!"
 | ((a==1 && b==3 && c==X) && (a==2 && b==3 && c==X) && (a==3 && b==3 && c==X)) = "You Won!"
 | ((a==1 && b==1 && c==X) && (a==1 && b==2 && c==X) && (a==1 && b==3 && c==X)) = "You Won!"
 | ((a==2 && b==1 && c==X) && (a==2 && b==2 && c==X) && (a==2 && b==3 && c==X)) = "You Won!"
 | ((a==3 && b==1 && c==X) && (a==3 && b==2 && c==X) && (a==3 && b==3 && c==X)) = "You Won!"
 | ((a==1 && b==1 && c==X) && (a==2 && b==2 && c==X) && (a==3 && b==3 && c==X)) = "You Won!"
 | ((a==1 && b==3 && c==X) && (a==2 && b==2 && c==X) && (a==1 && b==1 && c==X)) = "You Won!"
 | ((a==1 && b==1 && c==O) && (a==2 && b==1 && c==O) && (a==3 && b==1 && c==O)) = "You Lost!"
 | ((a==1 && b==2 && c==O) && (a==2 && b==2 && c==O) && (a==3 && b==2 && c==O)) = "You Lost!"
 | ((a==1 && b==3 && c==O) && (a==2 && b==3 && c==O) && (a==3 && b==3 && c==O)) = "You Lost!"
 | ((a==1 && b==1 && c==O) && (a==1 && b==2 && c==O) && (a==1 && b==3 && c==O)) = "You Lost!"
 | ((a==2 && b==1 && c==O) && (a==2 && b==2 && c==O) && (a==2 && b==3 && c==O)) = "You Lost!"
 | ((a==3 && b==1 && c==O) && (a==3 && b==2 && c==O) && (a==3 && b==3 && c==O)) = "You Lost!"
 | ((a==1 && b==1 && c==O) && (a==2 && b==2 && c==O) && (a==3 && b==3 && c==O)) = "You Lost!"
 | ((a==1 && b==3 && c==O) && (a==2 && b==2 && c==O) && (a==1 && b==1 && c==O)) = "You Lost!"
 | otherwise                                                                    = checkVictory ys

-}



