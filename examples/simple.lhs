This is a simple literate Haskell program demonstrating basic functions.

First, let's define a factorial function:

> factorial :: Integer -> Integer
> factorial 0 = 1
> factorial n = n * factorial (n - 1)

The factorial function uses recursion. Here's another example
with a helper function:

> fibonacci :: Integer -> Integer
> fibonacci n = fib n 0 1
>   where
>     fib 0 a _ = a
>     fib n a b = fib (n - 1) b (a + b)

This fibonacci implementation uses an accumulator pattern for efficiency.

> main :: IO ()
> main = do
>     print (factorial 5)
>     print (fibonacci 10)

That's the end of our simple program.
