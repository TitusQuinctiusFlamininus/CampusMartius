
{-# LANGUAGE ViewPatterns #-}


module Main where

import Hangman
import HangmanVisual

solutionword = "constable"

main :: IO ()
main = do putStrLn "Welcome To Haskell's Hangman"
          let mask      = hideWords solutionword
              initHang  = HangWord {uhang = mask, chances = length hangover} in 
               runHangman initHang 0


-- | Function into the wonderful world of hangman
runHangman :: HangWord -> HangStart -> IO ()
runHangman h s = 
             do guess <- getChar
                let ch = chances h
                    h' = guessLetter (guess, mapify solutionword) h in 
                             do  putStrLn ("Chances Left: "++(show ch))
                                 putStrLn ""
                                 putStrLn ""
                                 putStrLn ""
                                 putStrLn $ modProgress $ uhang h'
                                 putStrLn ""
                                 putStrLn ""
                                 putStrLn ""
                                 case chances h' == 0 of 
                                   True   -> do  putStrLn "              " 
                                                 putStrLn $ hangover !! s
                                                 putStrLn "´´´´´´´´´´´´´´" 
                                                 putStrLn "  GAME OVER   " 
                                                 putStrLn "``````````````" 
                                                 return ()
                                   False  -> do case chances h' == ch     of
                                                  True  -> runHangman h' s
                                                  False -> do putStrLn $ hangover !! s
                                                              putStrLn ""
                                                              putStrLn ""
                                                              putStrLn ""
                                                              runHangman h' (s+1)

