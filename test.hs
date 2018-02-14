{-
--Name: Jiahui Wang
--KUID: 2586742
--Assignment: Homework 5
--Date: 11/20/2015
-}

----------Part 2-----------------
module Philosophers where
 
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random


main = do
  forks <- mapM newTMVarIO [1..5]
  let namedPhil  = map starting philosophers
      forkPairs  = zip forks (tail . cycle $ forks)
      gotForksPhil = zipWith ($) namedPhil forkPairs
 
  mapM_ forkIO gotForksPhil

philosophers :: [String]
philosophers = ["Josh", "Cindy", "Amy", "Mia", "Lexi"]

type Fork = TMVar Int

-- Pick up the fork 
takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork

--Put down the fork
putFork :: Int -> Fork -> STM ()
putFork i fork = putTMVar fork i

 
starting :: String -> (Fork, Fork) -> IO ()
starting name (leftFork, rightFork) = forever $ do
                        putStrLn (name ++ " is waiting.")
 
                        (leftNum, rightNum) <- atomically $ do
                                  leftNum <- takeFork leftFork
                                  rightNum <- takeFork rightFork
                                  return (leftNum, rightNum)
 
                        putStrLn (name ++ " got forks " ++ show leftNum ++ " && " ++ show rightNum ++ " and is now eating.")
                        delay <- randomRIO (1,10)
                        threadDelay (delay * 1000000) 
                        putStrLn (name ++ " is done eating.")
 
                        atomically $ do
                                   putFork leftNum leftFork
                                   putFork rightNum rightFork
 
                        delay <- randomRIO (1, 10)
                        threadDelay (delay * 1000000)
 
 

{-
-----Test---------
Josh is done eating.
Lexi got forks 5 && 1 and is now eating.
Josh is waiting.
Amy is done eating.
Cindy got forks 2 && 3 and is now eating.
Lexi is done eating.
Mia got forks 4 && 5 and is now eating.
Amy is waiting.
Lexi is waiting.
Cindy is done eating.
Josh got forks 1 && 2 and is now eating.
Mia is done eating.
Amy got forks 3 && 4 and is now eating.
Josh is done eating.
Lexi got forks 5 && 1 and is now eating.
Cindy is waiting.
Josh is waiting.
Amy is done eating.
Cindy got forks 2 && 3 and is now eating.
Mia is waiting.
Lexi is done eating.
Mia got forks 4 && 5 and is now eating.
Cindy is done eating.
Josh got forks 1 && 2 and is now eating.

------Part 3--------
--Explain the differences between the two (possible) solutions.
--(Consider what the deadlock possibilities both your solutions have.)


STM is more powerful than MVar, since it can deal with a block of actions as a transaction by using 'atomically'. When the program enter the block, then other threads cannot see any execution until it's done.

--Why is STM more powerful than MVar?

-}
 
 
        