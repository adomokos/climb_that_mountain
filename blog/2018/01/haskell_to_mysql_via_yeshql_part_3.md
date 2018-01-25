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

In this next section let's verify the client create logic. Creating a record in a database is easy, we already verified it when we ran the app. However, making this automated and repeatable shows some challanges. We need to make sure that every test cleans after itself in the DB. We could wrap each and every spec in a transaction and just roll it back, but that would be quite complex. Dropping and rebuilding the database is fast as it is. Sure, it's a couple of hundred milliseconds, but that is negligable for now.

HSpec provides before hooks, we will hook into that.

Let's change the `test/Hashmir/DataSpec.hs` like this:

```haskell
module Hashmir.DataSpec where

import Test.Hspec
import System.Process
import qualified Hashmir.Data as D

main :: IO ()
main = hspec spec

resetDB :: IO ()
resetDB = callCommand "make build-db"

spec :: Spec
spec = before resetDB $ do
    describe "Hashmir Data" $ do
        it "runs a test" $ do
            clientId <- D.insertClient "TestClient" "testclient"
            clientId `shouldBe` 1
```

We call `resetDB` with every single spec, that function makes a system call to rebuild the DB.

When you try executing the test, stack tries to recompile the app, but it presents an error:

```shell
test/Hashmir/DataSpec.hs:4:1: error:
    Failed to load interface for ‘System.Process’
    It is a member of the hidden package ‘process-1.4.3.0’.
    Perhaps you need to add ‘process’ to the build-depends in your .cabal file.
```

Oh-oh. We need to add the `process` package to our test-suite, let's modify the `package.yml` like this:

```yaml
tests:
  hashmir-test:
    source-dirs: test/
    main: Spec.hs
    dependencies:
      - process # highlighted
      - hashmir
      - hspec == 2.*
    other-modules:
      Hashmir.DataSpec
```

After adding the `process` package, regenerating the cabal file, we can now run our first test successfully:

```shell
Hashmir.Data
  Hashmir Data
Dropping and rebuilding database hashmir_test
    runs a test

Finished in 0.1378 seconds
1 example, 0 failures
```

The beauty of this solution is that we can run it over and over again, the test will pass as the checked `clientId` will always be 1, since the database is recreated every time.

[This is](https://github.com/adomokos/hashmir/commit/af99147ab05146dcf37954886d0f23e3cef79bfd) the commit up to this point.
