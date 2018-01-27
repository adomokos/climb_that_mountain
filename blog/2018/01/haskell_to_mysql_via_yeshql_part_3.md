### Haskell to MySQL via YeshQL (Part 3.)


In the [previous](/blog/2017/12/haskell_to_mysql_via_yeshql_part_2.md) blog post we built a console app in Haskell that talks to MySQL via [YeshQL](https://github.com/tdammers/yeshql). It creates a client record and counts them with a SQL query.
In the final part in this series, we will add automated tests to our application, we will save a user record along with its parent client and make sure all the saving happens in one unit of work.

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

`make build` should recompile the app, and `stack test` will run the entire test suite.

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

#### Verify Client Create Logic

Creating a record in a database is easy, we already verified it when we ran the app. However, making this automated and repeatable shows some challenges. We need to make sure that every test cleans after itself in the DB. We could wrap each and every spec in a transaction and just roll it back, but that would be quite complex. Dropping and rebuilding the database is fast as it is. Sure, it's a couple of hundred milliseconds, but that is negligible for now.

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
        it "creates a Client record" $ do
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

[This is](https://github.com/adomokos/hashmir/commit/be20905a5ad5199c7b7b5e1eb4de8705a3656770) the commit up to this point.

#### Add a User Record Along With Client

Let's add a failing spec for this first. Add the following content to the `test/Hashmir/DataSpec.hs` file:

```haskell
    it "creates a Client and a User record" $ do
        clientId <- D.insertClient "TestClient" "testclient"
        userId <- D.insertUser clientId "joe" "joe@example.com" "password1"
        userId `shouldBe` 1
```

There is no `insertUser` function, let's add it. We also need to add the SQL template to the YeshQL code. It's very similar to the Client insert script, here are all the changes for that:

```haskell
[yesh|
    -- name:countClientSQL :: (Int)
    SELECT count(id) FROM clients;
    ;;;
    -- name:insertClientSQL
    -- :client_name :: String
    -- :subdomain :: String
    INSERT INTO clients (name, subdomain) VALUES (:client_name, :subdomain);
    ;;;
    -- name:insertUserSQL
    -- :client_id :: Integer
    -- :login :: String
    -- :email :: String
    -- :password :: String
    INSERT INTO users (client_id, login, email, password)
    VALUES (:client_id, :login, :email, :password);
|]
```

And the `insertUser` function is like this:

```haskell
insertUser :: Integer -> String -> String -> String -> IO Integer
insertUser clientId login email password =
    withConn $ insertUserSQL clientId login email password
```

When I run `make test`, this is the output printed on the screen:

```shell
Hashmir.Data
  Hashmir Data
Dropping and rebuilding database hashmir_test
    creates a Client record
Dropping and rebuilding database hashmir_test
    creates a Client and a User record

Finished in 0.2642 seconds
2 examples, 0 failures
```
The lines `Dropping and rebuilding database hashmir_test` is too much noise, let's remove it from the Makefile.

```shell
Hashmir.Data
  Hashmir Data
    creates a Client record
    creates a Client and a User record

Finished in 0.2354 seconds
2 examples, 0 failures
```

This looks much cleaner.

[Commit point](https://github.com/adomokos/hashmir/commit/63c976e618fe9e9b9ca1c833ad052f62a7d3486b) for this section.

#### Roll Back Transactions When Error Occurs

The happy path of our application is working well: the User and Client records are inserted properly. First, the Client is saved, its `id` is used for the User record to establish the proper references. But we should treat these two inserts as one unit of work: if the second fails, it should roll back the first insert.

Let's write a test for it. I'll make the created Client's `id` intentionally wrong by incrementing it by one.

```haskell
    it "rolls back the transaction when failure occurs" $ do
        clientId <- D.insertClient "TestClient" "testclient"
        _ <- D.insertUser (clientId + 1) "joe" "joe@example.com" "password1"
        clientCount <- D.withConn $ D.countClientSQL
        clientCount `shouldBe` Just 0
```

When I run the tests, this is the error I am getting:

```shell
Hashmir.Data
  Hashmir Data
    creates a Client record
    creates a Client and a User record
    rolls back the transaction when failure occurs FAILED [1]

Failures:

  test/Hashmir/DataSpec.hs:23:
  1) Hashmir.Data, Hashmir Data, rolls back the transaction when failure occurs
       uncaught exception:
           SqlError (SqlError {seState = "",
               seNativeError = 1452,
               seErrorMsg = "Cannot add or update a child row: a foreign key constraint
                             fails (`hashmir_test`.`users`, CONSTRAINT `client_id`
                             FOREIGN KEY (`client_id`) REFERENCES `clients` (`id`))"})

Randomized with seed 668337839

Finished in 0.3924 seconds
3 examples, 1 failure
```

The database is protecting itself from an incorrect state, a User record won't be saved with an `id` that does not match a record in the `clients` table. This exception is justified, although, it could be handled better with a Maybe type, that's not the point right now. Let's just expect this exception for now to see a proper test failure.

Change the test like this:

```haskell
    it "rolls back the transaction when failure occurs" $ do
        clientId <- D.insertClient "TestClient" "testclient"
        (D.insertUser (clientId + 1)
                      "joe"
                      "joe@example.com"
                      "password1")
            `shouldThrow` anyException
        clientCount <- D.withConn $ D.countClientSQL
        clientCount `shouldBe` Just 0
```

The spec now produces the error I would expect:

```shell
Failures:

  test/Hashmir/DataSpec.hs:31:
  1) Hashmir.Data, Hashmir Data, rolls back the transaction when failure occurs
       expected: Just 0
        but got: Just 1

Randomized with seed 1723584293

Finished in 0.3728 seconds
3 examples, 1 failure
```

Finally, we have a spec that fails correctly, as we are not rolling back the created Client record.

The reason the Client record is not rolled back is that we use two different transactions to persist the records: first, the Client record saved and the connection is committed, and then the User record is attempted to be saved. It fails, the record is not created, but the Client record has already been committed to the database. This is our problem, we should reuse the same connection for both save operations, and only commit it after the second one.

Let's refactor the code to do that. Both the `insertClient` and `insertUser` now accept a connection:

```haskell
insertClient :: H.IConnection conn =>
                      String -> String -> conn -> IO Integer
insertClient name subdomain =
    insertClientSQL name subdomain

insertUser :: H.IConnection conn =>
                    Integer -> String -> String -> String -> conn -> IO Integer
insertUser clientId login email password =
    insertUserSQL clientId login email password
```

The specs now has to be modified to pass in the connection:

```haskell
spec :: Spec
spec = before resetDB $ do
    describe "Hashmir Data" $ do
        it "creates a Client record" $ do
            clientId <- D.withConn $ D.insertClient "TestClient" "testclient"
            clientId `shouldBe` 1
        it "creates a Client and a User record" $ do
            userId <- D.withConn (\conn -> do
                clientId <- D.insertClient "TestClient" "testclient" conn
                D.insertUser clientId "joe" "joe@example.com" "password1" conn)
            userId `shouldBe` 1
        it "rolls back the transaction when failure occurs" $ do
            (D.withConn (\conn -> do
                clientId <- D.insertClient "TestClient" "testclient" conn
                D.insertUser (clientId+1) "joe" "joe@example.com" "password1" conn))
                `shouldThrow` anyException
            clientCount <- D.withConn $ D.countClientSQL
            clientCount `shouldBe` Just 0
```

And finally, the Main function has to be updated as well:

```haskell
main :: IO ()
main = do
    clientId <- D.withConn $ D.insertClient "TestClient" "testclient"
    putStrLn $ "New client's id is " ++ show clientId
    Just clientCount <- D.withConn D.countClientSQL
    putStrLn $ "There are " ++ show clientCount ++ " records."
```

When you run the tests, they should all pass now.

[Commit point](https://github.com/adomokos/hashmir/commit/b488dad7c610a84ba84e90e0340d0449ac094d3a) for this section.

#### Summary

In this blog series we set up YeshQL, and added logic to insert Client and its dependent User records. We added tests and made sure all the writes are in one transaction.

Our final solution works, but it requires the connection to be passed in. A Reader Monad would be a more elegant solution, but that should be a new blog post.
