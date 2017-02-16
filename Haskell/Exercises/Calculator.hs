--Addition Calculator: Will attempt to calculate a list of numbers from a list provided through the standard input
--will use the State Transformation monad to store intermediate results as state

import Data.Char
import Control.Monad.Trans(lift)
import Control.Monad.Trans.State.Lazy (StateT, put, execStateT)

type Vsf = Int --the value so far 
type Computations = Int --number of times we have added

type CalcState = (Vsf, Computations)

type Calculation a = StateT CalcState IO a


performCalc :: CalcState -> Calculation ()
performCalc calc@(i,n) = do
                           lift $ putStrLn "Put in a number or 'end' if you are done"
                           userInput <- lift $ getLine
                           if userInput /= "end" then
                              let newstate = (i+(read userInput :: Int), n+1) in
                              performCalc newstate
                           else put calc


main :: IO()
main = do
         do val <- execStateT (performCalc (0,0)) (0,0)
            putStrLn (show val)