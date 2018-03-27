
--functions to illustrate the use of continuations
--
--
--this function will create continuations from a list, given a fulcrum


createContinuation :: (Num a, Ord a) => a -> [a] -> [(a -> a)]
createContinuation  _     [] = []
createContinuation  fulc inp = map ((*)) . filter (< fulc) $ inp

--this function will create a suspended computation from a single input
suspend :: a  -> ((a -> a) -> a)
suspend x      = ($ x)

main :: IO ()
main           = putStrLn $ show answer
                 where answer = map (suspend 34 ) . createContinuation 5 $ [1,2,1,2,0,2,4,5,2,4,6,1,0]
                










