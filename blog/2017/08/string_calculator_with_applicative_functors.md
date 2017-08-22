### String Calculator with Applicative Functors

I like the simplicity of the [String Calculator kata](http://link...). It's a typical example of map-reduce algorithm, where the string has to be split by a delimiter, mapped into a list of integeres and then reduced to their sum. I've often used it as an example to quickly evaluate engineering candidates, try new languages and tools. This was the first challange I've tried to solve in Haskell about a year ago. The other day I found the code and I tried to see how I could improve upon it with everything I've learned so far.

This is what I found from a year before:

```haskell
import Test.Hspec
import Data.List.Split (splitOn)

calculator :: String -> Integer
calculator x = sum $ coerceString x ","

coerceString :: String -> String -> [Integer]
coerceString x c = map (coerce) (splitOn c x)
    where
        coerce "" = 0
        coerce a = read a :: Integer

main :: IO ()
main = hspec $ do
    describe "String Calculator" $ do
        it "returns 0 for empty string" $ do
            calculator "" `shouldBe` 0
        it "returns 1 for '1'" $ do
            calculator "1" `shouldBe` 1
        it "returns 3 for '1,2,3'" $ do
            calculator "1,2,3" `shouldBe` 6
        it "returns 0 for '1,2,!'" $ do
            calculator "1,2,!" `shouldBe` 0
```

This works really well for simple cases, but what should I do when there is a non-numeric string in the input arguments? Returning zero would be a reasonable solution, but Haskell can do better.

```haskell
        it "returns 0 for '1,2,!'" $ do
            calculator "1,2,!" `shouldBe` 0
```

The algorithm crashes as I suspected:

```shell
String Calculator
  returns 0 for empty string
  returns 1 for '1'
  returns 3 for '1,2,3'
  returns 0 for '1,2,!' FAILED [1]

Failures:

  string_calculator_01.hs:22:
  1) String Calculator returns 0 for '1,2,!'
       uncaught exception: ErrorCall (Prelude.read: no parse)

Randomized with seed 1943023196

Finished in 0.0023 seconds
4 examples, 1 failure
```

I could easily wrap the entire logic and return zero when an exception occurs, however, Haskell can do better. Much better.

I can return a `Maybe Int` from the operation that parses the string to an integer: if the result is `Nothing` here, the overall result will be `Nothing`. There is a string parser in the `Text.Read` module that does just that, it's called `readMaybe`.

Here is how it works:

```shell
 % ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/adomokos/.ghci
λ> import Text.Read
λ> (readMaybe "12") :: Maybe Int
Just 12
λ> (readMaybe "!") :: Maybe Int
Nothing
λ>
```
We need to parse the list of strings, which is achieved by `readMaybe`.

```shell
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/adomokos/.ghci
λ> import Text.Read (readMaybe)
λ> import Data.List.Split (splitOn)
λ> let xs = "1,2,3"
λ> splitOn (",") xs
["1","2","3"]
λ> map (\x -> readMaybe x :: Maybe Int) $ splitOn (",") xs
[Just 1,Just 2,Just 3]
```
We now have a list of Maybe values that can be reduced into a single Maybe value.

