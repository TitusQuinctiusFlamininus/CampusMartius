--Haskell TicTacToe

import Data.Char
import Control.Monad.Trans.State.Lazy(StateT, put, get, runStateT)
import Control.Monad.Trans.Reader(ReaderT, runReaderT)
import Control.Monad.IO.Class(liftIO)

type T3Cell =  (Int, Int, Char)
type T3Config = [[(Int, Int)]]
type TicTacToe a  = StateT [T3Cell] (ReaderT T3Config IO) a

board = [(1,1,'-'),(2,1,'-'),(3,1,'-'),(1,2,'-'),(2,2,'-'),(3,2,'-'),(1,3,'-'),(2,3,'-'),(3,3,'-')]
victoryindexes = [[(1,1),(2,1),(3,1)],[(1,2),(2,2),(3,2)],[(1,3),(2,3),(3,3)],
                  [(1,1),(1,2),(1,3)],[(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],
                  [(1,1),(2,2),(3,3)],[(1,3),(2,2),(3,1)]]

replaceCellInBoard :: T3Cell -> [T3Cell] -> [T3Cell]
replaceCellInBoard (a,b,i) board = map (\(x,y,z) -> if (x==a && y==b && z/='O' && z/='X') then (x,y,i)  else (x,y,z)) board

botPlayMove :: [T3Cell] -> [T3Cell]
botPlayMove board =
    let firstempty@(x,y,z) = head (filter (\(_,_,e) -> e == '-') board) in
        replaceCellInBoard (x,y,'O') board

isGameOver :: T3Config -> [T3Cell] -> Bool
isGameOver config board
 | config == [] = False
 | (all (\(_,_,e) -> e == 'X' || e == 'O') board) = True
 | otherwise =
   let ([(x1,y1),(x2,y2),(x3,y3)]:zs) = config
       indcheck = (filter (\(a,b,c) -> (x1==a && y1==b) || (x2==a && y2==b) || (x3==a && y3==b)) board) in
         if (all (\(_,_,v) -> v=='X') indcheck) || (all (\(_,_,v) -> v=='O') indcheck)
             then True
         else isGameOver zs board

runTicTacToe :: TicTacToe ()
runTicTacToe = do
    board <- get
    liftIO $ putStrLn $ show board
    liftIO $ putStrLn "Put an 'X' on the board (Give Entry as: (x-coord, y-coord)"
    (a:b:c:zs) <- liftIO $ getLine
    put (replaceCellInBoard (digitToInt(a), digitToInt(c), 'X') board)
    usermodified <- get
    if isGameOver victoryindexes usermodified
       then put usermodified
    else do 
       put (botPlayMove usermodified)
       botmodified <- get
       if isGameOver victoryindexes botmodified
          then put botmodified
       else runTicTacToe

main :: IO()
main = do
    result <- runReaderT (runStateT runTicTacToe board) victoryindexes
    putStrLn (show (snd result))
