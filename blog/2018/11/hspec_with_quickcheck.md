### Haskell's Hspec with QuickCheck

This blog post will not describe what property testing is, there are other writings about it. Instead, I'll try to show you how to get started with it, as finding introductory tutorials on the topic is scarce, this is my attempt to fill that void.

I heard about property-based testing in 2015, when I attended Michael Nygaard's StrangeLoop [talk](https://www.youtube.com/watch?v=N5HyVUPuU0E). As a long time TDD-er, I was highly skeptical of generating tests for my own code, but the idea started to grow on me.

Where would I use property testing in a real project? I was presented with a task at my job to solve this: "Write QuickCheck tests for a loan amortization algorithm. It has a starting amount, a term length (in month) and an interest rate. The test should verify:

* The loan is paid back
* The term length matches the input argument
* The principal is shrinking every month
"

I was eager to work on this task, however, practical examples were limited. The best information was in the book [Haskell Programming from First Principles](http://www.haskellbook.com). I kept coming back to its chapter on testing to pick up what I know.

I wanted to learn with a simple example, I chose the Roman Numeral kata. The converter has 2 functions:

* Convert Arabic to Roman (`convertToRoman`)
* Convert the Roman back to Arabic (`convertFromRoman`)

Following the [reverse list example](https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing) should be trivial: converting an Arabic number to Roman and back to Arabic should give the same number. The Haskell Book is using a similar example with morse code conversions.

I'll go through the good old TDD solution of the Roman Numeral kata in less detail, as the goal of this post is to describe how to use QuickCheck.

Here is how the code starts out:

```haskell
module RomanNumeralsSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

type Roman = String

convertToRoman :: Int -> Roman
convertToRoman 1 = "I"
convertToRoman 2 = "II"
convertToRoman 3 = "III"
convertToRoman 4 = "IV"

convertFromRoman :: Roman -> Int
convertFromRoman "" = undefined

spec :: Spec
spec =
  describe "Converting to Roman Numerals" $ do
    it "converts 1 to I" $
      convertToRoman 1 `shouldBe` "I"
    it "converts 2 to II" $
      convertToRoman 2 `shouldBe` "II"
    it "converts 3 to III" $
      convertToRoman 3 `shouldBe` "III"
    it "converts 4 to IV" $
      convertToRoman 4 `shouldBe` "IV"
```
[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/75f040310891559cb6eb46e28ae3a5f1be119f3b)

Look how dumb the test cases (and the code itself) are so far. When I introduce custom data types and a rule table for the conversions, the code becomes much simpler. Watch this:

```haskell
...

type Conversions = [(Int, Roman)]

conversions :: Conversions
conversions =
  [ (4, "IV")
  , (1, "I") ]

convertToRoman :: Int -> Roman
convertToRoman 0 = []
convertToRoman x =
  roman ++ convertToRoman (x - number)
    where
      (number, roman) =
        head . filter (\(a,_) -> a <= x) $ conversions

...
```
[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/76693b3da61eae126a6c70e76630ae54e266100e)

Extending this now is easy, I don't have to modify the logic, just add more values to the conversions table.

```haskell
...

conversions :: Conversions
conversions =
  [ (90, "XC")
  , (50, "L")
  , (40, "XL")
  , (10, "X")
  , (9, "IX")
  , (5, "V")
  , (4, "IV")
  , (1, "I") ]

...

spec :: Spec
spec =
  describe "Converting to Roman Numerals" $ do
    it "converts 1 to I" $
      convertToRoman 1 `shouldBe` "I"
    it "converts 2 to II" $
      convertToRoman 2 `shouldBe` "II"
    it "converts 3 to III" $
      convertToRoman 3 `shouldBe` "III"
    it "converts 4 to IV" $
      convertToRoman 4 `shouldBe` "IV"
    it "converts 5 to V" $
      convertToRoman 5 `shouldBe` "V"
    it "converts 6 to VI" $
      convertToRoman 6 `shouldBe` "VI"
    it "converts 8 to VIII" $
      convertToRoman 8 `shouldBe` "VIII"
    it "converts 9 to IX" $
      convertToRoman 9 `shouldBe` "IX"
    it "converts 10 to X" $
      convertToRoman 10 `shouldBe` "X"
    it "converts 11 to XI" $
      convertToRoman 11 `shouldBe` "XI"
    it "converts 99 to L" $
      convertToRoman 99 `shouldBe` "XCIX"
```
[Commit point](https://github.com/adomokos/roman-numerals-hs/commit/11ad84525034d586bf3e6127078bf70a6b26b9ef)

Oh, these tests are ugly. I'll make them a bit worse before I improve them, sorry about that.

The conversions table can be used to convert Roman numbers to Arabic. The Arabic value is accumulated by adding the matched values together.

Here are the conversion code and the accompanying tests:

```haskell
...
import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)
...

convertFromRoman :: Roman -> Int
convertFromRoman "" = 0
convertFromRoman r =
  number + convertFromRoman (drop (length roman) r)
    where
      (number, roman) = fromJust $ find (\(_,r') -> r' `isPrefixOf` r) conversions

  ...

    describe "Roman to Number Conversions" $ do
    it "converts I to 1" $
      convertFromRoman "I" `shouldBe` 1
    it "converts II to 2" $
      convertFromRoman "II" `shouldBe` 2
    it "converts III to 3" $
      convertFromRoman "III" `shouldBe` 3
    it "converts IV to 4" $
      convertFromRoman "IV" `shouldBe` 4
    it "converts V to 5" $
      convertFromRoman "V" `shouldBe` 5
    it "converts VIII to 8" $
      convertFromRoman "VIII" `shouldBe` 8
    it "converts IX to 9" $
      convertFromRoman "IX" `shouldBe` 9
    it "converts X to 10" $
      convertFromRoman "X" `shouldBe` 10
    it "converts XI to 11" $
      convertFromRoman "XI" `shouldBe` 11
    it "converts XCIX to 99" $
      convertFromRoman "XCIX" `shouldBe` 99
```

[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/e23149db832b3e1d83115e42063febd2f903ae35)

I ended up with 47 lines of mindless, verbose and repeated test code.

Let's fire up GHCi, and see how this works in the REPL. Use the Makefile target [make repl-test](https://github.com/adomokos/roman-numerals-hs/blob/master/Makefile#L17-L19) to try this:

```terminal
λ> convertToRoman 12
"XII"
λ> convertFromRoman . convertToRoman $ 12
12
```

OK, I can convert an Arabic number to Roman and convert that back to Arabic, I'll use this mechanism to verify the logic with QuickCheck.

Let's explore QuickCheck in the REPL. Please follow along, but notice, that the generated random numbers are going to be different for you.

```terminal
λ> import Test.QuickCheck
λ> sample (arbitrary :: Gen Int)
0
-1
-3
-4
-2
-9
8
14
11
-18
-16
```

It works for Strings as well:

```terminal
λ> sample (arbitrary :: Gen String)
""
"Q\EOT"
"#8"
"^x"
"&}\t\NULU k"
"\DC4\816800<"
"~!oH\763194&e\GSG"
"\STX\869030\194889\1040760\820031\799098w\SIHz"
"\47452\ETX>j\686979\ACK?\1094610\160069\943268\99807+\519462"
""
"\694257s\883691\996507fO_n4m\tF\357900"
```

Ok, this is great, but what if you just want random numbers between 1 and 3? Use the `elements` function:

```terminal
λ> let oneThroughThree = elements [1..3] :: Gen Int
oneThroughThree :: Gen Int
λ> sample' oneThroughThree
[3,1,3,3,3,1,2,1,3,2,2]
```

Note that I used the `sample'` function, which returns a list of elements now.

In case you want to have three with more frequencies, just provide a list accordingly. Like this:

```terminal
λ> let moreThrees = elements [1,2,3,3,3,3] :: Gen Int
moreThrees :: Gen Int
λ> sample' moreThrees
[1,3,3,3,3,2,3,2,1,1,3]
```

In the sample list the 3s are represented with higher frequencies.

But what if you want a combination of Strings and Integers for your needs? Well, you can build a short function for that:

```haskell
import Test.QuickCheck
...
genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
```

Fire up the REPL with the test code (I used the included Makefile's `repl-test` target), and sample this new function to get a list of tuples with Integers and Strings:

```terminal
λ> import Test.QuickCheck
λ> sample' (genTuple :: Gen (Int, String))
[(0,""),(-1,"P!"),(0,"\703651\888426O"),(-5,"\DLEs_\640436>\a"),(-6,""),(10,"T\45432\&5?\STX.j"),(-10,"h\266318\SUB\175378"),(9,"\ESC\978066"),(16,"FQ@w;'I^\EM\NUL"),(7,""),(12,"\ACK\569630\49462")]
```

[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/c25a9e4f45bd78f067d87c8c9f19f389ea3104da)

After this brief intro, let's replace the manual tests with generated tests by QuickCheck.

This is the function I came up with to test the logic:

```haskell
prop_convertNumber :: Int -> Bool
prop_convertNumber x = (convertFromRoman . convertToRoman) x == x
```

I gave it a try in the REPL:

```terminal
λ> import Test.QuickCheck
λ> quickCheck prop_convertNumber
*** Failed! Exception: 'Prelude.head: empty list' (after 3 tests and 1 shrink):
-1
λ> quickCheck prop_convertNumber
*** Failed! Exception: 'Prelude.head: empty list' (after 6 tests and 1 shrink):
-1
```

Oh, crap... QuickCheck generated `-1`, but my logic only works with positive numbers. I have to tell QuickCheck what numbers it can use.

[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/d872da41f55c120d58d18d686fce90578d3490af)

A valid range needs to be passed to QuickCheck to exercise the number conversion logic, the `elements` function could do just that. I add these two functions to my test:

```haskell
numbers :: [Int]
numbers = [1..1000]

genNumbers :: Gen Int
genNumbers = elements numbers
```

I could easily verify in the REPL that the `genNumbers` function is producing numbers within the defined range:

```terminal
λ> import Test.QuickCheck
λ> sample' genNumbers
[745,853,321,678,436,711,825,593,441,900,315]
λ> sample' genNumbers
[706,110,263,36,807,589,555,444,60,261,116]
```

Great, only positive numbers are generated, I can use this set for my tests. I change the property testing function to use the `genNumbers` generated values in the test:

```haskell
prop_convertNumber :: Property
prop_convertNumber =
  forAll genNumbers
    (\x ->
      (convertFromRoman . convertToRoman) x == x)
```

This works as expected when I test it in the REPL:

```terminal
λ> import Test.QuickCheck
λ> quickCheck prop_convertNumber
+++ OK, passed 100 tests.
```

I can now replace all my test with this function:

```haskell
spec :: Spec
spec = do
  describe "Converting to Roman Numerals" $ do
    it "converts number to Roman and back" $ property $
      prop_convertNumber
```

[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/9c3a49bc1216d87cf833945a164883ab1c636d40)

Hspec's QuickCheck wrapper provides a convenience function for scenarios like this: I can replace the `it` and `property` functions with `prop` like this (I need to add an import to make this work):

```haskell
import Test.Hspec.QuickCheck (prop)

...

spec :: Spec
spec = do
  prop "converts number to Roman and back" $
    prop_convertNumber
```

When I run the tests, they should all pass, and QuickCheck reports back that it generated 100 tests for me.

```terminal
RomanNumerals
  converts number to Roman and back
    +++ OK, passed 100 tests.

Finished in 0.0017 seconds
1 example, 0 failures
```

[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/1cb16e3bfe61eee4ab7f8c03f682dec338bbd848)

It's great that QuickCheck generates the numbers and runs these tests, however, I would like to look under the hood and see how these conversions are working. I'd like to eyeball what the converted Roman numbers are looking like.

This is all pure code, I can't just print numbers in the terminal. I also don't want to make pure code dirty with IO. `Debug.Trace` is the solution.

Check this out:

```haskell
import Debug.Trace
...

prop_convertNumber :: Property
prop_convertNumber =
  forAll genNumbers
    (\x ->
      traceShow("number: ", (x, convertToRoman x)) $
        (convertFromRoman . convertToRoman) x == x)
```

`traceShow` will print out both the Arabic and the converted Roman number.

Here is a sample of what I received:

```terminal
...
("number: ",(825,"XCXCXCXCXCXCXCXCXCXV"))
("number: ",(662,"XCXCXCXCXCXCXCXXXII"))
("number: ",(246,"XCXCLXVI"))
("number: ",(921,"XCXCXCXCXCXCXCXCXCXCXXI"))
    converts number to Roman and back
          +++ OK, passed 100 tests.

          Finished in 0.0030 seconds
          1 example, 0 failures
```

Oh, that 825 does not look like a valid Roman number. Of course: my conversion table only has values from 1 through 90. It can only convert numbers up to 98. I need to make sure QuickCheck will generate numbers within this range.


```haskell
numbers :: [Int]
numbers = [1..98]
```

When I run the tests, I got lucky by QuickCheck, it used 98 for one of the conversions:

```terminal
...
("number: ",(98,"XCVIII"))
("number: ",(62,"LXII"))
("number: ",(54,"LIV"))
  converts number to Roman and back
      +++ OK, passed 100 tests.

      Finished in 0.0034 seconds
      1 example, 0 failure
```

All good, now!

[Commit Point](https://github.com/adomokos/roman-numerals-hs/commit/ef4a61866d8f9c75b0236aafd344c7618dddfb9c)

QuickCheck has a little brother, called [SmallCheck](https://github.com/feuerbach/smallcheck), which will generate numbers up-to a defined depth. It might be a better fit for the Roman Numeral Kata, but that would be a different blog post. I encourage you to explore that library and see how you could change the test code to work with it.
