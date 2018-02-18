{-# LANGUAGE ExistentialQuantification #-}

module ProfunctorTry where

import Data.Profunctor 


data L a b   = forall s . L (s -> b) (a -> s)


instance Profunctor L where
 dimap h g (L result spin) = L (g. result) (spin . h)
 
 
runTheP :: L Int String -> Int -> String
runTheP (L result spin) input  = result . spin $ input

spinner :: Int -> [String]
spinner i = replicate i $ "thetruth"

resulter :: [String] -> String
resulter = concat 

mything :: Int -> L Int String
mything how = dimap  (const how) (reverse) (L resulter spinner)


main = runTheP (mything 7) 5
