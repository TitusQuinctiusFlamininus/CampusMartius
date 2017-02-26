import Control.Monad.Trans.Reader
import Data.Map as M
import Data.Maybe as May

type Bindings = Map String Int;

myDb = [("val1", 34), ("val2", 89),("val3", 4),("val4", 1007),("val5", 678)]

process :: String -> Reader Bindings Int
process key = do
           env <- ask
           let val1 = May.fromJust (M.lookup key env) in
               return (val1)

main :: IO()
main = do
         let finale = runReader (process "val4") (M.fromList myDb) in
             putStrLn (show finale)
