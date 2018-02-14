{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Monad.State

data Lit = Val Int | Const String | Empty deriving (Eq, Show)

data Op = Plus | Minu | Mult | Divi | Log | L_Par | R_Par | OpBottom deriving (Eq, Show)

data Order = Unary | Binary | Null | Bottom

nary :: Op -> Order
nary op = case op of
     Plus -> Binary
     Minu -> Binary
     Mult -> Binary
     Divi -> Binary
     Log -> Unary
     OpBottom -> Bottom
     _ -> Null

priority :: Op -> Int
priority op = case op of
         Plus -> 1
         Minu -> 1
         Divi -> 2
         Mult -> 2
         Log -> 3
         _ -> 0

type LitOp = Either Lit Op

type Stack = ([LitOp], [LitOp])

evaluate :: Op -> LitOp -> LitOp -> State Stack ()
evaluate op (Left (Val f1)) ( Left ( Val f2)) = case op of
         Plus -> push $ lv (f1 + f2)
         Minu -> push $ lv (f1 - f2)
         Mult -> push $ lv (f1 * f2)
         Divi -> push $ lv (f1 / f2)
evaluate op (Left (Val f1)) ( Left Empty) = case op of
Log -> push $ lv (logBase 2 f1)


lv :: Int -> LitOp
lv x = Left $ Val x
