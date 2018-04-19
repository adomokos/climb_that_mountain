### Path Count

I've bumped into this brain teaser recently:

"Given two integer numbers describing an n by m graph, where n represents the height and m represents the width, calculate the number of ways you can get from the top left to the bottom right if you can only go right and down."

That's a fine challenge, and prior to knowing recursive types in Haskell, I don't know how I would have approached the problem.

Let's draw what the paths would look like first.

Given 1x1, the case is pretty simple:

```haskell
{-
  The is what the matrix would look like:

  (0,1) - (1,1)
    |       |
  (0,0) - (1,0)
-}
```
The only way to get from the top left to the bottom right is to "walk" the perimeters:

```haskell
{-
  (0,1) - (1,1) - (1,0)
  (0,1) - (0,0) - (1,0)
-}
```

This case is so easy, I don't even bother with devising a solution. Let's look at a 1 by 2 graph:

```haskell
{-
  (0-1) - (1,1) - (2,1)
    |       |       |
  (0-0) - (1,0) - (2,0)
-}
```

Following the rules laid out, there are 3 ways to get from the top left to the bottom right point:

```haskell
{-
  (0-1) - (1,1) - (2,1) - (2,0)
  (0-1) - (1,1) - (1,0) - (2,0)
  (0-1) - (0,0) - (1,0) - (2,0)
-}
```

The rule "you can only go right and down" tells us something: it's a binary tree. How could I draw up a recursive tree structure for this?

I'd like to make sure the logic is correct, I put all this logic into an HSpec file. How 'bout this?

```haskell
-- test/PathCountSpec.hs

import Test.Hspec

main :: IO ()
main = hspec spec

type Point = (Int, Int)
data Tree a = Leaf
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

spec :: Spec
spec =
    describe "Path Count" $ do
        it "can calculate tree branches for 1x2 matrix" $ do
            let tree =
                    Node (0,1)
                        (Node (1,1)
                            (Node (2,1) Leaf
                                        (Node (2,0) Leaf Leaf))
                            (Node (1,0) (Node (2,0) Leaf Leaf)
                                        Leaf))
                        (Node (0,0)
                            (Node (1,0)
                                (Node (2,0) Leaf Leaf)
                                Leaf)
                            Leaf)
            {-
               Possible paths:
                (0,1) - (1,1) - (2,1) - (2,0)
                (0,1) - (1,1) - (1,0) - (2,0)
                (0,1) - (0,0) - (1,0) - (2,0)
            -}
            pending
```

The key here, that the number of times `Node (2,0) Leaf Leaf` appears is the number of different ways I can get from the top left to the bottom right. All I have to do is counting the number of times this sub-tree appears in the tree itself.

I wrote a function (that I put into the HSpec file itself) to do just that:

```haskell
leafCount :: Tree Point -> Int
leafCount Leaf = 0
leafCount (Node _ Leaf Leaf) = 1
leafCount (Node _ left right) = leafCount left + leafCount right
```

When I call this function with the provided tree I have in the spec, I should receive 3. This assertion passes:

```haskell
leafCount tree `shouldBe` 3
```

I manually had to build up this tree, next I looked at how I could generate it myself based on the top left and bottom right points. I had to make sure I won't add branches outside of the matrix, what I accomplished with two guards.

```haskell
buildTree :: Point -> Point -> Tree Point
buildTree (a,b) end@(c,d)
    | a > c = Leaf
    | b < 0 = Leaf
    | otherwise = Node (a, b) (buildTree (a+1,b) end) (buildTree (a,b-1) end)
```

This logic will keep "walking" right and down until the guards stop it.

I add another assertion to make sure this still passes:

```haskell
buildTree (0,1) (2,0) `shouldBe` tree
```
This is still passing. I can even combine the two:

```haskell
buildTree (0,1) (2,0) `shouldBe` tree
leafCount (buildTree (0,1) (2,0)) `shouldBe` 3
```

I want to make sure this logic still holds when I pivot the two numbers. This is the next assertion I added:

```haskell
buildTree (0,2) (1,0) `shouldBe` tree
```

Tests are passing. All right, I need to set up an outer function that takes two numbers and returns the number of paths.

Here it is:

```haskell
pathCount :: Int -> Int -> Int
pathCount m n = leafCount $ fromTree m n
    where fromTree m n = buildTree (0,m) (n,0)
```

This assertion will exercise this function:

```haskell
pathCount 1 2 `shouldBe` 3
```

Everything is green!

I add one more test to make sure the 1x1 graph works:

```haskell
pathCount 1 1 `shouldBe` 2
```

And finally, I add a 2x2 test as well. This one has 6 different paths to get from the top left to the bottom right:

```haskell
{-
    Possible paths:
    (0,2) - (1,2) - (2,2) - (2,1) - (2,0)
    (0,2) - (1,2) - (1,1) - (2,1) - (2,0)
    (0,2) - (1,2) - (1,1) - (1,0) - (2,0)
    (0,2) - (0,1) - (1,1) - (2,1) - (2,0)
    (0,2) - (0,1) - (1,1) - (1,0) - (2,0)
    (0,2) - (0,1) - (0,0) - (1,0) - (2,0)
-}
pathCount 2 2 `shouldBe` 6
```

And when I run the spec again, it all works! You can find the solution in [this gist](https://gist.github.com/adomokos/fd26a07d8c19f96905828a0670029f5b).
