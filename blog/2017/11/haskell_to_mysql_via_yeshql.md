### Haskell to MySQL via Yeshql (part 1)

As I was looking for an easy and light way to talk to Postgres from Clojure, I discovered [yesql](https://github.com/krisajenkins/yesql). I wanted to find a way to talk to a MySQL database and I found [yeshql](https://github.com/tdammers/yeshql). It's a template parsing library on top of [HDBC](https://github.com/ryantm/hdbc-mysql), exatly what I needed to keep SQL and Haskell code separate.

This blog post will show you how you can easily get yeshql up and running and run queries against MySQL. I'll build a simple CRUD console app, you should be able to take that and use code snippets in your apps.

The Clojure tutorial I created for my blog posts is named [Kashmir](https://github.com/adomokos/kashmir), I'll name this project Hashmir. You can follow along the evolution of the code via commit points, I'll post a link after each section.

You will need [stack](https://docs.haskellstack.org/en/stable/README/) and ghc installed, I have stack `Version 1.5.1 x86_64 hpack-0.17.1` and ghc version 8.0.2. MySQL is also needed, I have version 5.7.20, but I won't use anything fancy as far as the database goes, if you have MySQL installed, I am sure that will do it.

#### Generate the project with stack

Generate the project with stack: `stack new hashmir`. Go inside the directory, build and deploy the app with `stack build && stack install`. Run the skeleton app:

```shell
 % ~/.local/bin/hashmir-exe
someFunc
```
[Commit point](https://github.com/adomokos/hashmir/commit/de07eebdf3b0f40b550279a58231603eef4f4809)

#### Using hpack

Since I [learned](https://academy.mondaymorninghaskell.com/p/your-first-haskell-project) about [hpack](https://github.com/sol/hpack), I never touch a Haskell project's cabal file any more. This blog post assumes you are familiar with this tool, feel free to learn about hpack more before you proceed.
This is the hpack file I add to the project to generate the various sections in the cabal file.

Add this `package.yml` to the project's root directory:

```shell
name: hashmir
version: 0.1.0.0
author: Attila Domokos <adomokos@gmail.com>
maintainer: adomokos@gmail.com
copyright: 2017 Attila Domokos
category: Console App
homepage: https://github.com/adomokos/hashmir#readme

ghc-options: -Wall

dependencies:
  - base >= 4.7 && < 5

library:
  source-dirs: src/
  exposed-modules:
    - Lib

executables:
  hashmir-exe:
    source-dirs: app/
    main: Main.hs
    dependencies:
      hashmir
    ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

Feel free to use your name and Github repo in this file.
Delete and regenarate the project's cabal file with this command: `rm -f hashmir.cabal && stack build`. `stack install` should produce the same executable file.

[Commit point](https://github.com/adomokos/hashmir/commit/873bacf0c76787e9e199f994ca43d6de2f67cf3a)

#### Setting up the Database

We will need a MySQL user and a database we can use in this project. Let's add a Makefile to script that out for us. You might need the master user in MySQL to create a role and grant access.

[This is](https://github.com/adomokos/hashmir/blob/master/resources/schema.sql) the schema file we will work with. I won't include it in this post, but this script should drop and recreate the tables. Put it into the `./resources` directory.

These targets in the [Makefile](https://github.com/adomokos/hashmir/blob/c8fb2b66f07b0cb6cb79e7a22d4f7715218dd960/Makefile) will create the role and will rebuild the database:

```shell
create-db-user: ## Creates a DB user with the root MySQL user
	mysql -u root --host $(HOST) -e "CREATE USER '$(DBUSER)'@'$(HOST)' IDENTIFIED BY '$(DBPASSWD)';" > /dev/null 2>&1
	mysql -u root --host $(HOST) -e "GRANT ALL PRIVILEGES ON `$(DBNAME)`.* TO '$(DBUSER)'@'$(HOST)';" > /dev/null 2>&1

build-db: ## Builds the DB
	@echo "Dropping and rebuilding database $(DBNAME)"
	@mysql -u $(DBUSER) --password='$(DBPASSWD)' --host $(HOST) -e "DROP DATABASE IF EXISTS $(DBNAME);" > /dev/null 2>&1
	@mysql -u $(DBUSER) --password='$(DBPASSWD)' --host $(HOST) -e "CREATE DATABASE $(DBNAME);" > /dev/null 2>&1
	@mysql -u $(DBUSER) --password='$(DBPASSWD)' --host $(HOST) $(DBNAME) < resources/schema.sql > /dev/null 2>&1
```
In case your MySQL root user uses a password, you can put it into the `~/.my.cnf` file or just modify the Makefile and provide it there.

You should be able to execute `make build` to rebuild the app, and `make run` to rebuild the DB and run the app:

```shell
% make run
Dropping and rebuilding database crud_yeshql_test
time ~/.local/bin/hashmir-exe
someFunc

real	0m0.011s
user	0m0.002s
sys	0m0.007s
```

[Commit point](https://github.com/adomokos/hashmir/commit/ff40249ac7bc767e9f57b52daccf690cbf3bae1a)

#### Writing the First Query

There are two parts of yeshql's code:
1. The SQL templates
2. Code that uses the generated functions from the template

Modify the `app/Main.hs` file like this:

```haskell
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}

module Main where

import Database.YeshQL
import Database.HDBC.MySQL

[yesh|
    -- name:countClientSQL :: (Int)
    SELECT count(id) FROM clients;
|]

getConn :: IO Connection
getConn = do
    connectMySQL defaultMySQLConnectInfo {
        mysqlHost     = "localhost",
        mysqlDatabase = "hashmir_test",
        mysqlUser     = "hashmir_user",
        mysqlPassword = "shei7AnganeihaeF",
        mysqlUnixSocket = "/tmp/mysql.sock"
    }

countClient :: IO ()
countClient = do
    conn <- getConn
    Just (clientCount) <- countClientSQL conn
    putStrLn $ "There are " ++ show clientCount ++ " records."

main :: IO ()
main = countClient
```

When you try to build the project (I conventiently use `make build`) one of these errors are displayed:

```shell
Failed to load interface for ‘Database.YeshQL’
```

I started referencing `Database.YeshQL`, however, I did not add that library to the project. This is where hpack is helpful, we only have to add it to the `package.yaml` file, that will generate a cabal file with the correct references.

Let's modify the `dependencies` section of package.yaml file like this:

```yaml
...

dependencies:
  - base >= 4.7 && < 5
  - yeshql
  - HDBC
  - HDBC-mysql

...
```

When I try to build the project, I get the following error:

```shell
Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for hashmir-0.1.0.0:
    yeshql must match -any, but the stack configuration has no specified version (latest applicable is 3.0.1.3)
needed since hashmir-0.1.0.0 is a build target.

Recommended action: try adding the following to your extra-deps in ~/hashmir/stack.yaml:
- yeshql-3.0.1.3
```

By modifying the `extra-deps: []` array like this in `stack.yaml`
```yaml
extra-deps: [
    yeshql-3.0.1.3
]
```
will download yeshql for the project and build it successfully.

When I run the app (use `make run` to do it), this is what I see:

```shell
% make run
Dropping and rebuilding database hashmir_test
time ~/.local/bin/hashmir-exe
There are 0 records.

real	0m0.019s
user	0m0.009s
sys	0m0.006s
```

Yeshql generated a function on-the-fly, named `countClientSQL`. That's the function I invoked in the `countClient` function. Since there are no records in the table, 0 was returned from the function.

[Commit point](https://github.com/adomokos/hashmir/commit/78a597e2c348abe751178812367f260fde69edb6)

We set up the project, ran the first query against MySQL from Haskell via yeshql templates. This brings us to the end of part 1 of this series. In the next article we'll start adding more SQL queries to insert and queryvarious records.
