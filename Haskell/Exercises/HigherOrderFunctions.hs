data Me  a  = Me  a deriving (Show)
data You a  = You a deriving (Show)
data Us  a  = Us  a deriving (Show)

f1 :: Me Int -> You Int
f1 (Me a) = You (a + 3)

f2 :: You Int -> Us Int
f2 (You a) = Us (a * 6)

f3 :: Us Int -> Me Int
f3 (Us a) = Me (a -1)
 

hof1 :: Me Int -> (You Int -> Us Int) -> Us Int
hof1 thatsme g2 =  g2 . f1 $ thatsme 

hof2 :: (Me Int -> You Int) -> Us Int -> Us Int
hof2 g3 ushere =  f2. g3 . f3 $ ushere

hof3 :: Me Int -> Us Int
hof3  = f2. f1

hof4 :: Us Int -> You Int
hof4 = f1. f3

hof5 :: (Me Int -> You Int)
hof5 = hof3 . hof2



main :: IO ()
main = do return () 
--         putStrLn $ show (hof1 (Me 5))  -- (Me 47)
         --putStrLn $ show (hof2 (Me 5))  -- (Us 48)
         --putStrLn $ show (hof3 (Me 5))  -- (Us 48)
