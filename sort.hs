{-# LANGUAGE GADTs, KindSignatures #-}

import Data.List
import Data.Char
import Debug.Trace


insertX :: Ord a => a -> [a] -> [a]
insertX x [] = [x]
insertX x (y:ys) = if x < y then x:y:ys
                   else y:insertX x ys

             
deleteY :: Eq a => a -> [a] -> [a]
deleteY x [] = error "list is empty"
deleteY x ys = if elem x ys then 
          let index = (\(Just i) -> i) (elemIndex x ys)
            in let (ms,ns) = splitAt index ys
              in ms ++ (tail ns)
              else ys


{-Data constructor of Tree contain Node and Leaf-}
data Tree :: *where
     Node :: Tree -> Tree -> Tree
     Leaf :: Int -> Tree deriving (Show)

{-split a list to a tuple of list, it will be used to build balanced tree further-}
split :: (Ord a) => [a] -> ([a], [a])
split list = do
           if ((length list) `mod` 2) == 0 then firstlist 
           else secondlist
            where
                 middlePoint = (length list) `div` 2
                 front1 = take middlePoint list
                 front2 = take (middlePoint + 1) list
                 back1 = drop middlePoint list
                 back2 = drop (middlePoint + 1) list
                 firstlist = (front1, back1)
                 secondlist = (front2, back2)

{-transfer the Tree structor to list-}
transfer :: Tree -> [Int]
transfer (Leaf x) = [x]
transfer (Node leftTree rightTree) = (transfer leftTree) ++ (transfer rightTree)

{-delete a node from the tree and build a new tree, it uses recursion-}
deleteNode :: Int -> Tree -> Tree
deleteNode x tree = do
                     let toList = transfer tree
                       in let newList = deleteY x toList
                          in balanceTree newList
                          
{-Insert a node to the tree and build a new tree-}
insertNode :: Int -> Tree -> Tree
insertNode x tree = do
                    let toList = transfer tree
                        in let newList = insertX x toList
                           in balanceTree newList
                           
--store elements a at the leaves                
balanceTree :: [Int] -> Tree
balanceTree list = do 
                if (length list) == 1 then atree
                else if (length list) == 2 then btree
                else 
                      let listTotuple = split list
                         in let leftlist = fst listTotuple
                               in let rightlist =  snd listTotuple
                               in Node (balanceTree leftlist) (balanceTree rightlist) 
                where atree = Leaf (head list)
                      btree = Node (Leaf (head list)) (Leaf (last list))

{-Get the height of tree-}
height :: Tree -> Int
height (Leaf _) = 0
height (Node left right) =  ((max (height left)  (height right)) +1)
                            
{-Get the total values of tree-}
sizeOftree :: Tree -> Int
sizeOftree (Leaf a) = 1
sizeOftree (Node t1 t2) = (sizeOftree t1) + (sizeOftree t2)

 
{-how to duplicate a string with given value times, it will be used to print tree structure-}
doubleSpace :: Int -> String
doubleSpace a = replicate (2 * a) ' '

{-Print the tree, and count the depth of node, which determine how many space(the level of node) before a node-}
printHelper :: Tree -> Int -> String
printHelper (Leaf a) depth = (doubleSpace depth)*2 ++ "Leaf: " ++ show a
printHelper (Node t1 t2) depth = doubleSpace depth ++  "Node: \n" ++ (printHelper t1 (depth + 1)) ++ "\n" ++ (printHelper t2 (depth + 1))
              

printTree :: Tree -> String
printTree tree = printHelper tree 0


{-It used for storing the list value-}
preorder :: Tree -> [Int]
preorder (Leaf a) = [a]
preorder (Node t1 t2) = (preorder t1 ++ preorder t2)

{-Find the the certain leaf with given index-}
findNode :: Int -> Tree -> Int
findNode index (Leaf a) = a
findNode index (Node t1 t2) = (preorder t1 ++ preorder t2) !! (index-1)
            
{-Find the minimum and maximum value of a tree-}
minmax :: Tree -> (Int, Int)
minmax (Leaf a) = (a, a)
minmax (Node t1 t2) = (min (fst a) (fst b), max (snd a) (snd b))
                      where a = minmax t1
                            b = minmax t2
                      
       
main = putStrLn $ printTree (balanceTree [2,3,1,4,6,8])       
                   



