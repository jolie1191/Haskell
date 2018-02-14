{-
---Name: Jiahui Wang
---KUID: 2586742
---Assignement: HW4
---Date: 11/05/2015
-}

{-# LANGUAGE GADTs, KindSignatures #-}


import Control.Applicative
import Control.Monad

{---------PART 0----------------
1. What is the Functor function, with its type? (fmap)

class Functor f where
     fmap::(a->b) -> f a -> f b

---Functor is a typeclass, 'a' and 'b' are type variable, 'f' is type constructor, and then 'f a' becomes a new type. For example, if 'f' is 'Maybe', 'a' can be Int, Float, etc. (Maybe Int), or (Maybe Float) could be new type. 

---'fmap' is similar to 'map'

map::(a->b) -> [a] -> [b]

---Compare above code with fmap, 'f a' in fmap is really similar to '[a]' in map. If we let 'f' be 'List'
, then 'f a' is 'List a' (that is [a]), then 'fmap' is instantiated to 'map', 'map' is the special case of 'fmap'. Here 'List' is the instance of Functor.

instance Functor [] where
    fmap = map


test:

ghci>fmap (*3) [1,2,3]
[3,6,9]
ghci>map (*3) [1,2,3]
[3,6,9]

=========================
2. What are the Applicative functions, with there types? ((<*>),pure)

Applicative Functor is a Functor, which defined in the Control.Applicative. 

class (Functor f) => Applicative f where
    pure:: a -> f a
    (<*>):: f (a -> b) -> f a -> f b

-- From above defination, Applicative is a typeclass, if one type is the Applicative instance, we could define some action on this type. 'pure' receive a value, return an Applicative value, for example, 'pure' recieve a value of Int, and then return the value of type Maybe Int.

-- <*> recieve a value of Functor(that is 'f (a -> b)'), and recieve another Functor value('f a'), then put the function(that is '(a -> b)' ) which is from the first Functor on the value of the second Funcotr('f a'), and finally get the another Functor('f b') 

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> sth = fmap f sth

-- 'pure' defines how a general type transform to an Apllicative value. In term of <*>, the fmap means that take a function and a Functor, then return another functor.

--Test---

ghci> (Just (*2)) <*> (Just 3)
Just 6

ghci> (pure (+1)) <*> (Just 3)
Just 4

===========================
3. What is the Monad functions, with there types? ((>>=),return)
 
class Monad m where
     return :: a -> m a
     (>>=) :: m a -> (a -> m b) -> mb

instance Monad Maybe where
     return x = Just x
     Nothing >>= f = Nothing

-- Monad is typeclass. 'return' can transform a general type value into a Monad value, for example, it can transform 5 into Just 5. In term of (>>=), it takes a monad value('m a') and a function('a -> m b') , then get the value('a') from the Monad value('m a') and pass the value('a') to the parameter of the function, finally we get another Monad value ('m b')

 ---------------------------}


{-----------Part 1---------------
Write fmap in terms of the applicative function <*> and pure.-}

fmap f x = pure f <*> x

{-Explain how your solution works.

- (pure f) is a functor which contains a function ( a -> b), <*> will extract that function from the functor (pure f) and then map the function to another functor.

-}


{--------------Part 2------------------}
{-• Write <*> in terms of liftA2.-}
{-• Write liftA2 in terms of <*>-}

LiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
LiftA2 f fa fb = pure f <*> fa <*> fb

myAp :: Applicative f => f (a -> b) -> f a -> f b
myAp fab fa = LiftA2 (\ g a -> g a) fab fa


{-• Explain how both solutions work.
Both of solutions work. 
For LiftA2, it maps a funciton over two functors and return a functor. For <*>, it takes a functor which contians a function(a -> b) and another functor, and then extract the function from the first functor and return a relative functor 'f b'.  

-}



{------------Part 3----------
There is a monad function, join.
join :: Monad m => m (m a) -> m a-}

{-• Write join using any monadic, applicative or functor functions.-}
join' :: Monad m => m (m a) -> m b
join' f = f >>= id

{-• Write >>= using join, without using >>=.-}

bind' :: Monad m => m a -> (a -> m b) -> m b
bind' m f = join (fmap f m)

{-• Explain how both solutions work.
For join', join extract monad (m a) from monad of monad (m (m a)) and return a monad (m a). 
For bind', bind takes a container of monad (m a) and a function (a -> m b) and return a monad (m b), the fmap will map the function over the contianer and return (m (m b)), and then we can use the join to get monad (m b).
-}

