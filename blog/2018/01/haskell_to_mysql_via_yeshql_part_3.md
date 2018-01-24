### Haskell to MySQL via YeshQL (Part 3.)


In the [previous](/blog/2017/12/haskell_to_mysql_via_yeshql_part_2.md) blog post we built a console app in Haskell that talks to MySQL via [YeshQL](https://github.com/tdammers/yeshql). It creates a client record and counts them with a SQL query.
In the final post in this series we will add automated tests to our application, we will save a user record along with its parent client and make sure all the saving happens in one unit of work.

[This is](https://github.com/adomokos/hashmir/commit/275d45ad1f6abe1f6f5eccb0e67c552543c96c90) the commit point where we left it at the end of Part 2.

#### Add Tests

Let's set up the great testing tool [HSpec](some_link_to_hspec) in our project.

First, replace the content of `test/Spec.hs` file with this:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

This will let auto-discover any spec files we have in the `test` directory.

Let's add the first test to the project in the `test/Hashmir/DataSpec.hs` file:

```haskell
module Hashmir.DataSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Hashmir Data" $ do
        it "runs a test" $ do
            True `shouldBe` True
```

We are not testing anything real here, I just want to make sure all the building blocks are in place.

Add the `test-suite` directive to `package.yml` file:

```yaml
...

tests:
  hashmir-test:
    source-dirs: test/
    main: Spec.hs
    dependencies:
      - hashmir
      - hspec == 2.*
    other-modules:
      Hashmir.DataSpec
```

`make build` should recomplie the app, and `stack test` will run the entire test suite.

When all is good, you should see this:

```shell
Hashmir.Data
  Hashmir Data
    runs a test

Finished in 0.0010 seconds
1 example, 0 failures
```

It wouldn't save much typing, but I like navigating the projects I am working on from a Makefile, I added these changes to run the tests with `make test`:

```shell
...
test: ## Run the specs
  @stack test

.PHONY: help test
```

[This is](https://github.com/adomokos/hashmir/commit/9f7c14e6aa518da44338b7426822173053ecf6c0) the commit point for this section.


