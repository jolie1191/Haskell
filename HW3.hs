{-# LANGUAGE GADTs, KindSignatures #-}

import Data.Char

main :: IO ()
main = ask 

ask :: IO ()
ask = do
        putStrLn "Guess a number?"
        xs <- getLine
        loop (read xs)

{-
        putStrLn "How many bottles of irn bru are on the wall?" >>
        getLine >>= \ xs -> 
        loop (read xs)
-}

loop :: Int -> IO ()
loop i = do
        if i < 4 then putStrLn "Too Low" >> ask
        else if i > 4 then putStrLn "Too High" >> ask
        else putStrLn "You win a cookie"


{-


-}