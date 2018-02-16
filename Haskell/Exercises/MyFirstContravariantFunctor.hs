module MyFirstContravariantFunctor where

import Data.Functor.Contravariant 

data MyContraThing a b = MyContraThing { getIt :: (b -> a) }

instance Contravariant (MyContraThing a) where
 contramap f (MyContraThing c)      =   MyContraThing (c . f) -- the input (6) will be applied to f first, not c !
--also note: the function f must take (as input), things of the same type, as the function c
--that is: if c takes in Ints, then f must take Ints as well

doSomething :: MyContraThing [String] Int
doSomething = contramap (*2)  MyContraThing { getIt = (\i -> replicate i "here") }

runIt :: (MyContraThing [String] Int) -> Int -> [String]
runIt (MyContraThing f) = f

main :: IO ()
main = mapM_ putStrLn $ runIt doSomething 6  
