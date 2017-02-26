import Control.Monad.Trans.Reader
import Data.Map as M


type Bindings = Map String Int;

type Mydatabase a = Reader Bindings a

myDb = [("val1", 34), ("val2", 89),("val3", 4),("val4", 1007),("val5", 678)]

process :: String -> Mydatabase Int
process key = do
           env <- ask
           let lookedup = (M.lookup key env) in
            case lookedup of
              Nothing -> return 0
              Just z  -> return z

main :: IO()
main = do
         let finale = runReader (process "val4") (M.fromList myDb) in
             putStrLn (show finale)
