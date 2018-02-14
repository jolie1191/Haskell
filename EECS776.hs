{-# LANGUAGE GADTs, KindSignatures #-}
{- import System.IO
import Data.Array

--define type named state-->box is empty, full, wall
data State = Empty | Full | Wall
           deriving ( Eq, Ord, Show, Enum)

--define type named box -->two instance: cordinate(type: (Int Int)) and BoxSate (type: state)
--type box = Cordinate (Int Int) | BoxState state

type Box = ((Int, Int), State)

type Grid = [((Int, Int), State)]

--snake::[(Int, Int)]
grid :: Grid
grid = [((x,y), if (x == 1 || x == 10 || y == 1 || y == 10)  then  Wall else  Empty) | x<-[1..10], y<-[1..10] ]


type SnakeHead = Box

type SnakeTail = Box

type SnakeBody = [Box]

snake :: SnakeHead -> SnakeTail -> SnakeBody - > IO Grid
snake = 
-}

module Calculator where
import Control.Monad.State

data Lit = Value Int | COnst String | Empty deriving (Eq, Show)

data Operation = Plus | Minu | Mult | Divi | Log | L_Par | R_Par | DollarSign
      deriving (Eq, Show)

data Order = Unary | Binary | Null | Bottom

nary :: Operation -> Order
nary op = case op of
Plus -> Binary
Minu -> Binary
Mult -> Binary
Divi -> Binary
Log -> Unary
DollarSign -> Bottom
- -> Null



