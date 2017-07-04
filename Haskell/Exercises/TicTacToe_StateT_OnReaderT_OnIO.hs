--Haskell TicTacToe

import Data.Char
import Control.Monad.Trans.State.Lazy            (StateT, put, get, runStateT)
import Control.Monad.Trans.Reader                (ReaderT, runReaderT)
import Control.Monad.IO.Class                    (liftIO)

type T3Cell =  (Int, Int, Char)
type T3Config = [[(Int, Int)]]
type TicTacToe a  = StateT [T3Cell] (ReaderT T3Config IO) a

board = [(1,1,' '),(2,1,' '),(3,1,' '),(1,2,' '),(2,2,' '),(3,2,' '),(1,3,' '),(2,3,' '),(3,3,' ')]
victoryindexes = [[(1,1),(2,1),(3,1)],[(1,2),(2,2),(3,2)],[(1,3),(2,3),(3,3)],
                  [(1,1),(1,2),(1,3)],[(2,1),(2,2),(2,3)],[(3,1),(3,2),(3,3)],
                  [(1,1),(2,2),(3,3)],[(1,3),(2,2),(3,1)]]

--Function to place an X on the board when user specifies a coordinate
replaceCellInBoard :: T3Cell -> [T3Cell] -> [T3Cell]
replaceCellInBoard _ []                    = []
replaceCellInBoard cell@(a,b,i) brd@((x,y,z):rs)
 | (x==a && y==b && z/='O' && z/='X')      = (x,y,i) : replaceCellInBoard cell rs
 | otherwise                               = (x,y,z) : replaceCellInBoard cell rs

--map (\(x,y,z) -> if (x==a && y==b && z/='O' && z/='X') then (x,y,i)  else (x,y,z)) brd

--Function for the computer to make a move
botPlayMove :: [T3Cell] -> [T3Cell]
botPlayMove brd =
    let (x,y,_) = head (filter (\(_,_,e) -> e == ' ') brd) in
        replaceCellInBoard (x,y,'O') brd

--Function to check if the game is over: if the board is completely full of Xs and Os or if there is some row of Xs or Os
isGameOver :: T3Config -> [T3Cell] -> Bool
isGameOver config brd
 | config == [] = False
 | (all (\(_,_,e) -> e == 'X' || e == 'O') brd) = True
 | otherwise =
   let ([(x1,y1),(x2,y2),(x3,y3)]:zs) = config
       indcheck = (filter (\(a,b,_) -> (x1==a && y1==b) || (x2==a && y2==b) || (x3==a && y3==b)) brd) in
         if (all (\(_,_,v) -> v=='X') indcheck) || (all (\(_,_,v) -> v=='O') indcheck)
             then True
         else isGameOver zs brd

--Function to get the entry on the board as a string
getEntry :: T3Cell -> [Char]
getEntry (_,_,v) = [v]

--Function to display the board on the commandline console
showBoard :: [T3Cell] -> IO ()
showBoard b =
    do putStrLn "   -   -   -    -  " ; putStr "|   ";putStr (getEntry (b!!6)); putStr "  |  ";putStr (getEntry (b!!7)); putStr "  |  ";putStr (getEntry (b!!8))
       putStrLn "  |  ";putStrLn "   -   -   -    -  ";putStr "|   ";putStr (getEntry (b!!3));putStr "  |  ";putStr (getEntry (b!!4));putStr "  |  ";putStr (getEntry (b!!5))
       putStrLn "  |  ";putStrLn "   -   -   -    -  ";putStr "|   ";putStr (getEntry (head b));putStr "  |  ";putStr (getEntry (b!!1));putStr "  |  ";putStr (getEntry (b!!2))
       putStrLn "  |  ";putStrLn "   -   -   -   -   "


--Function to play the game
runTicTacToe :: TicTacToe ()
runTicTacToe = do
    brd <- get
    liftIO $ showBoard brd
    liftIO $ putStrLn "Put an 'X' on the board (Hint: Bottom Left Square is 1,1)"
    input <- liftIO $ getLine
    if input /= ""
      then let (a:_:b:_) = input in
           put (replaceCellInBoard (digitToInt a, digitToInt b, 'X') board)
    else return ()
    usermodified <- get
    if isGameOver victoryindexes usermodified
       then liftIO $ showBoard usermodified
    else do
       put (botPlayMove usermodified)
       botmodified <- get
       if isGameOver victoryindexes botmodified
          then liftIO $ showBoard botmodified
       else runTicTacToe

main :: IO()
main = do
    putStrLn "----/ WELCOME TO HASKELL HIC-HAC-MISTLE-TOE /----"
    _ <- runReaderT (runStateT runTicTacToe board) victoryindexes
    putStrLn "GAME OVER"
