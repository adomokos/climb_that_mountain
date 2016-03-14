## Saturday, October 31, 2015

### [Clojure API with Yesql, Migrations and More (Part 1.)](http://www.adomokos.com/2015/10/clojure-api-with-yesql-migrations-and.html)

I've found endless books and articles explaining core ideas and the building blocks of the Clojure programming language. They show you how to use the different data structures, they have good examples for little tasks, like reading and parsing a CSV file, but books or articles that walk you through an example of building a comprehensive solution is hard to find.
I always liked writings that showed me how to build an app. I would learn many aspects of a language, get familiar with tools, and most of all, I would build something that could serve as foundation for my future projects.
I am planning to do just that with this series of blog posts. I'd like to show you how to:

*   Set up your database environment with scripts
*   Manage database changes through migrations
*   Test the different components
*   Stub out function calls you don't need for testing
*   Add logging to monitor the database communication
*   And all this in Clojure!

By the end of these blog posts you will be able to expose database through Clojure libraries as JSON data with HTTP endpoints.

I have following presumptions:

*   You have Clojure installed (I have version 1.7.0 at the time of writing)
*   We'll use PostgreSQL (mine is 9.4.4)
*   I am using OSX (10.10.5)

The name of the app is "kashmir", you can find the final solution in [this public repo](https://github.com/adomokos/kashmir). I will link specific commit points to the blog posts, this way you can join this tutorial at any point you want. Let's dive in!

#### The Data Model

The data model is simple, it has only 3 tables. The `members` table lists the various band members, the `bands` table lists all the bands those members belong to, and the `bands_members` table is used to map the members to their bands.
This is what it looks like:

![bands_members](/resources/2015/10/bands_members.png)

#### Creating the Database User

I use the excellent [pgcli](http://pgcli.com/) tool as my command line interface for Postgres. It has code completion, table name suggestion features, it's your `psql` tool on steorid. If you don't have it, grab it through homebrew. Create a DB user called "kashmir_user" and allow this user to create DBs. This is how you do it in the command line, all the inputs are highlighted:

```shell
% pgcli postgres
Version: 0.19.1
Chat: https://gitter.im/dbcli/pgcli
Mail: https://groups.google.com/forum/#!forum/pgcli
Home: http://pgcli.com
postgres> CREATE USER kashmir_user PASSWORD 'password1';
CREATE ROLE
Command Time: 0.000s
Format Time: 0.000s
postgres> ALTER USER kashmir_user CREATEDB;
ALTER ROLE
Command Time: 0.000s
Format Time: 0.000s
postgres>
```

#### Initializing the Project

Generate the new project skeleton by runnig the `lein new app kashmir` command in the terminal. You should have a skeleton app project that looks like [this](https://github.com/adomokos/kashmir/commit/70a6a45c6372bf5ab5cf02ec5c6e4a4853d44a77). When you run `lein run`, you see "Hello, World!", and when you run the tests you see the 1 failure:

```shell
% lein test

lein test kashmir.core-test

lein test :only kashmir.core-test/a-test

FAIL in (a-test) (core_test.clj:7)
FIXME, I fail.
expected: (= 0 1)
  actual: (not (= 0 1))

Ran 1 tests containing 1 assertions.
1 failures, 0 errors.
Tests failed.
```

#### Creating the Database

Database drop and create operations should be scripted. You can use `rake db:drop` and `rake db:create` in Rails, we should have something similar here. You can use the Postgres command line tools to create and drop databases with the `createdb` and `dropdb` commands. The `--if-exists` switch helps you when you're running it for the first time, the command won't error out if the database does not exist.
The easiest way to create executable tasks is with a Makefile. Create a new file called `Makefile` in your project root and add this to it:

```shell
DBNAME=kashmir
USER=kashmir_user
PORT=5432
HOST=localhost

PGPASSWORD=password1

# Builds the DB by dropping and recreating it
build-db:
 dropdb --if-exists --username $(USER) $(DBNAME) -h $(HOST) -p $(PORT)
 createdb --username $(USER) $(DBNAME) -h $(HOST) -p $(PORT)
```

We set up variables in the Makefile, it will be easy to change these values later, you are also adhering to good DRY principles.
Run the Make target by typing the command `make build-db` in the terminal. You can run this as many times as you want, it will drop and recreate the empty database for you.
([Commit point](https://github.com/adomokos/kashmir/commit/8fb99ec0b0f5bd21322c5ee1c8f270a486a49a55).)

#### Running Migrations

The best way to implement changes in a database is through reversible migrations. I usually use the great [clj-sql-up](https://github.com/ckuttruff/clj-sql-up) migration tool for that. Let's add it to the project.clj file:

```clojure
(defproject kashmir "0.1.0-SNAPSHOT"
  ...
  :clj-sql-up {:database "jdbc:postgresql://kashmir_user:password@localhost:5432/kashmir"
               :deps [[org.postgresql/postgresql "9.4-1201-jdbc4"]]}
  :plugins  [[clj-sql-up "0.3.7"]])
```

Run the command `lein clj-sql-up create create-members` to generate your first migration. This should create a new file in the "migrations" directory. Open up that file and add your migration SQL to it:

```clojure
(defn up []
    ["CREATE TABLE members(id SERIAL PRIMARY KEY,
                           first_name varchar(50) NOT NULL,
                           last_name varchar(50) NOT NULL,
                           email varchar(50) NOT NULL,
                           created_at timestamp NOT NULL default CURRENT_TIMESTAMP)"
     "CREATE INDEX idx_members_id ON members(id)"
     "CREATE UNIQUE INDEX idx_email_unique ON members(email)"])

(defn down []
  ["DROP TABLE members"])
```

Test your SQL by running `lein clj-sql-up migrate` in the terminal. I would recommend looking at the database to make sure the first table, "members" got created properly. Open up pgcli and run `\dt` from the pgcli prompt. You should see two tables listed there:

*   clj_sql_migrations
*   members

The table "clj_sql_migrations" is used to track the actual version of your database, it's the metadata for clj-sql-up to run the migrations. Let's add the "bands" and "bands_members" tables as well, create a new migration file with the clj-sql-up generator: `lein clj-sql-up create create-bands`. Open up the migrations/*-create-bands.clj file and add this SQL:

```clojure
(defn up []
    ["CREATE TABLE bands(id SERIAL PRIMARY KEY,
                         name varchar(50) NOT NULL,
                         created_at timestamp NOT NULL default CURRENT_TIMESTAMP)"
     "CREATE INDEX index_bands_id ON bands(id)"
     "CREATE TABLE bands_members(id SERIAL PRIMARY KEY,
                                 band_id INTEGER REFERENCES bands (id),
                                 member_id INTEGER REFERENCES members (id),
                                 created_at timestamp NOT NULL default CURRENT_TIMESTAMP)"])

(defn down []
  ["DROP TABLE bands"]
  ["DROP TABLE bands_members"])
```

We should be running the migrations when we drop and rebuild the database. Change your Makefile's "build-db" target like this:

```shell
...
# Builds the DB by dropping and recreating it
build-db:
  dropdb --if-exists --username $(USER) $(DBNAME) -h $(HOST) -p $(PORT)
  createdb --username $(USER) $(DBNAME) -h $(HOST) -p $(PORT)
  lein clj-sql-up migrate
```

Now when you run `make build-db` in your terminal, you should see the database recreated by dropping it first, creating it and running the migration at the end.  
([Commit point](https://github.com/adomokos/kashmir/commit/cfe053fbc712e7125b80a440f8578d02a5ea6b0b).)

In the [next post](/blog/2015/10/clojure-api-with-yesql-migrations-part2.md) we'll add seed data to the database for testing purposes, we will also use the excellent [Yesql](https://github.com/krisajenkins/yesql) library to communicate with the database.


POSTED BY ATTILA DOMOKOS AT 10:23 AM


NO COMMENTS
