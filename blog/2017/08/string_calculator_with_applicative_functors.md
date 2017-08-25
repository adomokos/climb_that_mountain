### String Calculator with Applicative Functors in Haskell

I like the simplicity of the [String Calculator kata](http://osherove.com/tdd-kata-1/). It's a typical example of the map-reduce algorithm, where the string has to be split by a delimiter, mapped into a list of integers and then reduced to their sum. I've often [used it](https://github.com/adomokos/stringcalulator_js_starter_kit) as an example to quickly evaluate engineering candidates, try new languages and tools. This was the first challenge I tried to solve in Haskell about a year ago. I found the code the other day, I wanted to see how I could improve upon it with everything I've learned so far.

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
```

This works for simple cases, but what should I do when there is a non-numeric string in the input arguments?

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

I can return a `Maybe Int` from the operation that parses the string to an integer: if the result is `Nothing` here, the overall result will be `Nothing`.
There is a string parser in the `Text.Read` module that does just that, it's called `readMaybe`.

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
I need to parse the list of strings, which is achieved by mapping over the values with `readMaybe`:

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
I now have a list of Maybe Int values that can be reduced into a single Maybe Int value. Reducing an array of numbers would be super easy (`foldl (+) 0 [1,2,3]` or `foldl1 (+) [1,2,3]` or just simply `sum [1,2,3]`). It's obvious that I will need something similar.

Adding a number to a `Maybe Int` can be achieved with a Functor:

```shell
λ> fmap (+10) (Just 4)
Just 14
λ> (+10) `fmap` (Just 4)
Just 14
λ> (+10) <$> Just 4
Just 14
```
All three expressions mean the same thing. The first is using the fmap as a conventional function name and arguments style, the second one uses the infix version of `fmap` and the third one is using a symbol.

This works for adding a number to a Maybe Int, however, I need to use an Applicative Functor to calculate the sum of two Maybe Ints.

```shell
λ> (+) <$> Just 10 <*> Just 4
Just 14
```

Using Applicative Functors, folding the list of Maybe Ints happens like this:

```shell
λ> let xs = [Just 1, Just 2, Just 3]
λ> foldl (\acc x -> (+) <$> x <*> acc) (Just 0) xs
Just 6
λ> foldl1 (\acc x -> (+) <$> x <*> acc) xs
Just 6
```

The solution now works, although a bit hard to read:

```haskell
import Test.Hspec
import Text.Read (readMaybe)
import Data.List.Split (splitOn)

calculator :: String -> Maybe Int
calculator input =
    foldr (\x acc -> (+) <$> x <*> acc) (Just 0) $
        map (\x -> readMaybe x) $ splitOn "," input

main :: IO ()
main = hspec $ do
    describe "String Calculator" $ do
        it "returns Nothing for empty string" $ do
            calculator "" `shouldBe` Nothing
        it "returns Just 1 for '1'" $ do
            calculator "1" `shouldBe` Just 1
        it "returns Just 3 for '1,2,3'" $ do
            calculator "1,2,3" `shouldBe` Just 6
        it "returns Nothing for '1,2,3,!'" $ do
            calculator "1,2,3,!" `shouldBe` Nothing
```

It's more meaningful after I've refactored it into chunks:

```haskell
calculator :: String -> Maybe Int
calculator input =
    let foldingFn acc x =  (+) <$> acc <*> x
        parsedInts = map (\x -> readMaybe x) . splitOn (",")
    in foldr1 (foldingFn) (parsedInts input)
```

The `parsedInts` function can be further simplified:

```haskell
calculator :: String -> Maybe Int
calculator input =
    let foldingFn acc x =  (+) <$> acc <*> x
        parsedInts = map (readMaybe) . splitOn (",")
    in foldr1 (foldingFn) (parsedInts input)
```

And finally, the sum of list of Maybe values can be calculated by using the `sequence` function like this:

```haskell

calculator :: String -> Maybe Int
calculator input =
    let parsedInts = map (readMaybe) . splitOn (",")
    in fmap sum . sequence $ parsedInts input
```

I find this form to be the most readable, but I liked the journey of getting there through the Applicative Functors.
