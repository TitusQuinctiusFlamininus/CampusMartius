--Haskell TicTacToe

import Data.Char
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State.Lazy(StateT, put, get, execStateT)

data T3Input = X | O | N deriving (Show)

type T3Cell =  (Int, Int, T3Input)

board = [(1,1,N),(2,1,N),(3,1,N),(1,2,N),(2,2,N),(3,2,N),(1,3,N),(2,3,N),(1,3,N)]

type TicTacToeState a = StateT [T3Cell] IO a

replaceCellInBoard :: T3Cell -> [T3Cell] -> [T3Cell]
replaceCellInBoard (a,b,i) board =
    map (\(x,y,z) -> if (x==a && y==b) then (x,y,i)  else (x,y,z)) board


--runTicTacToe :: TicTacToeState ()
--runTicTacToe = do
--                 theboard <- get
--                 cell@(x,y,i) <- getLine
--                 case i of
--                      'X'   -> put (replaceCell cell theboard)
--                      'O'   -> 
