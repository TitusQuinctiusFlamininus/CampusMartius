import Control.Monad.Trans.Reader
import Data.Map as M


type Bindings = Map String Int; -- how our configuration binding will be composed
type Mydatabase a = Reader Bindings a --the reader monad we will use to hold the binding

--typical configuration data
myDb = [("val1", 34), ("val2", 89),("val3", 4),("val4", 1007),("val5", 678)]

--function that takes a key and returns its value from the configuration, otherwise it returns zero
process :: String -> Mydatabase Int
process key = do
                env <- ask
                case (M.lookup key env) of
                  Just z    -> return z
                  otherwise -> return 0

--main function
main :: IO()
main = do
         finale <- return $ runReader (process "val4") (M.fromList myDb)
         putStrLn (show finale)
