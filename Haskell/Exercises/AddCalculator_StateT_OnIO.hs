--Addition Calculator: Will attempt to calculate a list of numbers from a list provided through the standard input
--will use the State Transformation monad to store intermediate results as state


import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Lazy (StateT, put, execStateT, get)

type Vsf = Int --the value so far
type Computations = Int --number of times we have added

type CalcState = (Vsf, Computations, Int)

type Calculation a = StateT CalcState IO a


performCalc :: Calculation ()
performCalc = do
                           (i,n,_) <- get
                           userInput <- lift $ getLine
                           if userInput /= "end" then
                              put (i+(read userInput :: Int), n+1,1)
                           else put (i,n,0)

doit :: CalcState -> IO ()
doit state = do
                   putStrLn "Put in a number or 'end' if you are done"
                   (a,b,c) <- execStateT performCalc state
                   if c == 0 then
                       putStrLn ("You added "++ (show b) ++ " numbers: The Total is : " ++ (show a))
                   else doit (a,b,c)


main :: IO()
main = do doit (0,0,1)
