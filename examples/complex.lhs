Advanced Literate Haskell Example
=================================

This demonstrates more complex formatting scenarios.

Data Types and Type Classes
---------------------------

> data Tree a = Empty | Node a (Tree a) (Tree a)
>    deriving (Show, Eq)

> instance Functor Tree where
>   fmap _ Empty = Empty
>   fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

Tree Operations
--------------

> insert :: (Ord a) => a -> Tree a -> Tree a
> insert x Empty = Node x Empty Empty
> insert x (Node a left right)
>   | x == a = Node x left right
>   | x < a  = Node a (insert x left) right
>   | x > a  = Node a left (insert x right)

> search :: (Ord a) => a -> Tree a -> Bool
> search _ Empty = False
> search x (Node a left right)
>   | x == a = True
>   | x < a  = search x left
>   | x > a  = search x right

Usage Examples
-------------

Here's how we might use these functions:

> exampleTree :: Tree Int
> exampleTree = foldr insert Empty [4,2,6,1,3,5,7]

> main :: IO ()
> main = do
>   print exampleTree
>   print $ search 3 exampleTree
>   print $ search 8 exampleTree
>   print $ fmap (*2) exampleTree