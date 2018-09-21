
{-# LANGUAGE ViewPatterns #-}


module Main where

import Hangman
import HangmanVisual

solutionword = "constable"

main :: IO ()
main = do putStrLn "Welcome To Haskell's Hangman"
          putStrLn ""
          putStrLn logo
          putStrLn ""
          
          let mask      = hideWords solutionword
              initHang  = HangWord {uhang = mask, chances = length hangover} in 
               do putStrLn ("You start with "++(show $ length hangover)++" Chances! ")
                  runHangman initHang 0


-- | Function into the wonderful world of hangman
runHangman :: HangWord -> HangStart -> IO ()
runHangman ((any (== '_') .  uhang) -> False) _  = putStrLn (saved++"    WORD =>["++solutionword++"]")
runHangman h s = 
             do putStrLn ("Guess a Letter : ")
                guess <- getLine
                putStrLn ""
                putStrLn ""
                let h' = guessLetter (if guess == [] then '$' else (head guess), mapify solutionword) h in 
                             do  case chances h' == 0 of 
                                   True   -> do  putStrLn "              " 
                                                 putStrLn ((hangover !! s) ++ "    WORD =>["++(modProgress $ uhang h')++"]")    
                                                 putStrLn "´´´´´´´´´´´´´´´´´´´´´´" 
                                                 putStrLn "  GAME OVER (loser!)  " 
                                                 putStrLn "``````````````````````" 
                                                 return ()
                                   False  -> do case chances h' == chances h  of
                                                  True  -> case s>1 of 
                                                            True  -> do showProgress (hangover !! (s-1))
                                                                                     (modProgress $ uhang h') (chances h')
                                                                        runHangman h' s
                                                            False -> do showProgress (hangover !! s)
                                                                                     (modProgress $ uhang h') (chances h')
                                                                        runHangman h' s
                                                  False -> do showProgress (hangover !! s) (modProgress $ uhang h')
                                                                           (chances h')
                                                              runHangman h' (s+1)

