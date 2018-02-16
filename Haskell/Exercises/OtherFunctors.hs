module OtherFunctors where

import Data.Functor.Contravariant 

data MyContraThing a b = MyContraThing { getIt :: (b -> a) }


instance Contravariant (MyContraThing a) where
 contramap f (MyContraThing c)      =   MyContraThing (c . f) 

somefunc :: Int -> [String]
somefunc input =  replicate input "here" 

mything = MyContraThing { getIt = somefunc }

producer :: Int -> Int
producer = (*2)

doSomething :: MyContraThing [String] Int
doSomething = contramap producer mything


runIt :: (MyContraThing [String] Int) -> Int -> [String]
runIt (MyContraThing f) = f

main :: IO ()
main = mapM_ putStrLn $ runIt mything 6