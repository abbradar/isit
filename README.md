# isit
Quasi-quoter to check if an expression satisfies a pattern

# Example
```
λ filter [is|Left (Just 4)|] [Right "test", Left (Just 4), Left Nothing]
[Left (Just 4)]
λ let x = 4
λ filter [is|Left (Just x)|] [Right "test", Left (Just 4), Left Nothing]
[Left (Just 4)]
λ :t [is|Just _|]
[is|Just _|] :: Maybe t -> Bool
```
