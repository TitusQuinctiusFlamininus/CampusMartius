module OtherFunctors where

import Data.Functor.Contravariant 

data MyContraThing a b = MyContraThing { getIt :: (b -> a) }

instance Contravariant (MyContraThing a) where
 contramap f (MyContraThing c)      =   MyContraThing (c . f) 

doSomething :: MyContraThing [String] Int
doSomething = contramap (*2)  MyContraThing { getIt = (\i -> replicate i "here") }

runIt :: (MyContraThing [String] Int) -> Int -> [String]
runIt (MyContraThing f) = f

main :: IO ()
main = mapM_ putStrLn $ runIt doSomething 6
