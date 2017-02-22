--Haskell TicTacToe

import Data.Char
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State.Lazy(StateT, put, get, execStateT)

data T3Input = X | O | N deriving (Show)

instance Eq T3Input where
    X == X = True
    O == O = True
    N == N = True
 

type T3Cell =  (Int, Int, T3Input)

board = [(1,1,N),(2,1,N),(3,1,N),(1,2,N),(2,2,N),(3,2,N),(1,3,N),(2,3,N),(1,3,N)]

type TicTacToeState a = StateT [T3Cell] IO a

replaceCellInBoard :: T3Cell -> [T3Cell] -> [T3Cell]
replaceCellInBoard (a,b,i) board =
    map (\(x,y,z) -> if (x==a && y==b) then (x,y,i)  else (x,y,z)) board

checkVictory :: [T3Cell] -> String
checkVictory board@((a,b,c):ys)
 | ((a==1) && (b==1) && (c==X)) && ((a==2) && (b==1) && (c==X)) && ((a==3) && (b==1) && (c==X)) = "You Won!"
 | (a==1 && b==2 && c==X) && (a==2 && b==2 && c==X) && (a==3 && b==2 && c==X) = "You Won!"
 | (a==1 && b==3 && c==X) && (a==2 && b==3 && c==X) && (a==3 && b==3 && c==X) = "You Won!"
 | (a==1 && b==1 && c==X) && (a==1 && b==2 && c==X) && (a==1 && b==3 && c==X) = "You Won!"
 | (a==2 && b==1 && c==X) && (a==2 && b==2 && c==X) && (a==2 && b==3 && c==X) = "You Won!"
 | (a==3 && b==1 && c==X) && (a==3 && b==2 && c==X) && (a==3 && b==3 && c==X) = "You Won!"
 | (a==1 && b==1 && c==X) && (a==2 && b==2 && c==X) && (a==3 && b==3 && c==X) = "You Won!"
 | (a==1 && b==3 && c==X) && (a==2 && b==2 && c==X) && (a==1 && b==1 && c==X) = "You Won!"
 | otherwise                                                                  = if (all (\(_,_,e) -> e == X || e == O) board) then "Noboby Wins, Too bad." else "Keep Playing!...."

--runTicTacToe :: TicTacToeState ()
--runTicTacToe = do
--                 theboard <- get
--                 cell@(x,y,i) <- getLine
--                 case i of
--                      'X'   -> put (replaceCell cell theboard)
--                      'O'   -> 
