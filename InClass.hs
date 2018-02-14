{-# LANGUAGE GADTs, KindSignatures #-}
import Data.IORef

fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x-1)

loop :: Int -> IO ()
loop n = do
   putStrLn $ "fac " ++ show n ++ "=" ++ show (fac n)
   if n <= 10 
   then loop (n+1)
   else return ()


loop2 :: IORef Int -> IO ()
loop2 r = do
n <- readIORef r
putStrLn $ "fac" ++ show n ++ " = " ++ show (fac n)
if n <= 10
then do writeIORef r (n + 1)
   loop2 r
else return ()