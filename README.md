foreign-store
=====

Store a stable pointer in a foreign context to be retrieved
later. Persists through GHCi reloads.

Example:

``` haskell
$ cabal repl
> store <- newStore ((*2) :: Int -> Int)
> store
Store 0
> :l Foreign.Store
Ok, modules loaded: Foreign.Store.
> store
<interactive>:5:1: Not in scope: `store'
> Just store <- lookupStore 0 :: IO (Maybe (Store (Int -> Int)))
> f <- readStore store
> print (f 6)
12
>
```
