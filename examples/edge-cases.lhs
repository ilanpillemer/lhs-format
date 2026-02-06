A literate Haskell file with edge cases.

This has some tricky scenarios:

> -- This is a comment
> {- And this is a 
>    multiline comment -}

Empty lines between prose and code:

> main = putStrLn "hello"

Some indented prose:
  - This is indented
  - Another indented line

> helper :: String -> String  
> helper s = reverse s   

Final prose section.
> lastFunction = id