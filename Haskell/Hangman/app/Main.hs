
{-# LANGUAGE ViewPatterns #-}


module Main where

import Hangman
import HangmanVisual

import Data.Char    (toUpper)

solutionword = "mississippi"

main :: IO ()
main = do putStrLn "Welcome To Haskell's Hangman"
          putStrLn ""
          putStrLn logo
          putStrLn ""
          
          let mask      = hideWords solutionword
              initHang  = HangWord {uhang = mask, chances = length hangover} in 
               do putStrLn ("You start with "++(show $ length hangover)++" Chances! ")
                  runHangman initHang 0 (mapify solutionword)


-- | Function into the wonderful world of hangman
runHangman :: HangWord -> HangStart -> Solution -> IO ()
runHangman ((any (== '_') .  uhang) -> False) _ _    = putStrLn (saved++"    WORD =>["++solutionword++"]")
runHangman h                                  s sol  = 
             do putStrLn ("Guess a Letter : ")
                guess <- getLine
                putStrLn ""
                putStrLn ""
                let theguess = if guess == [] then '$' else (head guess)
                    (h',sol') = guessLetter (theguess, sol) h in 
                                 do  case chances h' == 0 of 
                                       True   -> do  putStrLn "              " 
                                                     putStrLn ((hangover !! s) ++ "    WORD WAS => "++
                                                              (toUpper <$> solutionword)++"")    
                                                     putStrLn "´´´´´´´´´´´´´´´´´´´´´´" 
                                                     putStrLn "  GAME OVER (loser!)  " 
                                                     putStrLn "``````````````````````" 
                                                     return ()
                                       False  -> do case chances h' == chances h  of
                                                      True  -> case s>1 of 
                                                                True  -> do showProgress (hangover !! (s-1))
                                                                                         (modProgress $ uhang h') (chances h')
                                                                            runHangman h' s sol'
                                                                False -> do showProgress (hangover !! s)
                                                                                         (modProgress $ uhang h') (chances h')
                                                                            runHangman h' s sol'
                                                      False -> do showProgress (hangover !! s) (modProgress $ uhang h')
                                                                               (chances h')
                                                                  runHangman h' (s+1) sol'

