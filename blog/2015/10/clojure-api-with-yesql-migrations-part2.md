## Saturday, October 31, 2015

### [Clojure API with Yesql, Migrations and More (Part 2.)](http://www.adomokos.com/2015/10/clojure-api-with-yesql-migrations-part2.html)

In the [previous article](http://www.adomokos.com/2015/10/clojure-api-with-yesql-migrations-and.html), we started working on [kashmir](https://github.com/adomokos/kashmir), a Clojure project that interacts with a database, and exposes the data through a JSON HTTP endpoint.
In this post we'll seed the database with some test data, add [yesql](https://github.com/krisajenkins/yesql) as our DB communication tool, at the end we will cover testing.

#### Adding Seed Data

Use [this commit](https://github.com/adomokos/kashmir/commit/cfe053fbc712e7125b80a440f8578d02a5ea6b0b) as your starting point for this exercise. Rebuild your database by running `make build-db` to make sure you have no records in the tables. Create a new file in resources/seeds.sql and add the following content to it:

```sql
INSERT INTO bands(name) VALUES ('The Beatles');
INSERT INTO bands(name) VALUES ('The Doors');

INSERT INTO members(first_name, last_name, email)
VALUES ('John', 'Lennon', 'jlennon@beatles.com');
INSERT INTO members(first_name, last_name, email)
VALUES ('Paul', 'McCartney', 'pmccartney@beatles.com');
INSERT INTO members(first_name, last_name, email)
VALUES ('George', 'Harrison', 'gharrison@beatles.com');
INSERT INTO members(first_name, last_name, email)
VALUES ('Ringo', 'Starr', 'rstarr@beatles.com');

INSERT INTO bands_members(band_id, member_id) VALUES(1, 1);
INSERT INTO bands_members(band_id, member_id) VALUES(1, 2);
INSERT INTO bands_members(band_id, member_id) VALUES(1, 3);
INSERT INTO bands_members(band_id, member_id) VALUES(1, 4);
```

This will create 2 band and 4 member records. It will also associate the members of The Beatles with their band record. We will insert these records through Postgres' command line tool. Let's add this to our build-db target in our Makefile:

```shell
 ...
build-db:
 dropdb --if-exists --username $(USER) $(DBNAME) -h $(HOST) -p $(PORT)
 createdb --username $(USER) $(DBNAME) -h $(HOST) -p $(PORT)
 lein clj-sql-up migrate
 psql -U $(USER) -d $(DBNAME) --file resources/seeds.sql > /dev/null
```

We added `< /dev/null` to this line, we are not interested seeing how many records got inserted into the tables. When you run `make build-db` you should have the seed data inserted into your database.
([Commit point](https://github.com/adomokos/kashmir/commit/2990ad682f3d934275941b300aa453d2f116f738).)

#### Talking to the Database with Yesql

The natural way to communicate with a database in Clojure is using [java.jdbc](https://github.com/clojure/java.jdbc). However, the spaghetti SQL is hard to understand, and mixing of Clojure code with SQL could make it a mess very quickly. I found the fantastic tool [yesql](https://github.com/krisajenkins/yesql) a few weeks ago and it was just what I needed: an easy way to separate SQL from Clojure. Let's add yesql and the Postgres jdbc driver to the project by modifying the project.clj file this way:

```clojure
(defproject kashmir "0.1.0-SNAPSHOT"
  ...
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.postgresql/postgresql "9.4-1201-jdbc41"]
                 [yesql "0.5.1"]]
  ...)
```

Create a new directory called "sql" under "src/kashmir". Create a new SQL file in this directory called "data.sql". Add these two queries to it:

```sql
-- name: find-member-by-id
-- Find the member with the given ID(s).
SELECT *
FROM members
WHERE id = :id

-- name: count-members
-- Counts the number of members
SELECT count(*) AS count
FROM members
```

The line in this SQL file that begins with `-- name:` has special significance. Yesql will create data access functions with the name you define there.
Add a new Clojure file under "src/kashmir" called "data.clj", this file will hold the data access functions. Add the following code to it:

```ruby
(ns kashmir.data
  (:require [yesql.core :refer [defqueries]]
            [clojure.java.jdbc :as jdbc]))

(def db-spec {:classname "org.postgresql.Driver"
              :subprotocol "postgresql"
              :subname "//localhost:5432/kashmir"
              :user "kashmir_user"
              :password "password1"})

(defqueries "kashmir/sql/data.sql"
            {:connection db-spec})
```

I am a bit unhappy with duplicating the Postgres connection information here, I'll leave you to set up the DB connection in the project.clj file.
Fire up the REPL to see if this works (you can find my input highlighted below):

```shell
% lein repl
nREPL server started on port 55527 on host 127.0.0.1 - nrepl://127.0.0.1:55527
REPL-y 0.3.7, nREPL 0.2.10
Clojure 1.7.0
Java HotSpot(TM) 64-Bit Server VM 1.8.0_60-b27
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

kashmir.core=> (require '[kashmir.data :refer :all])
nil
kashmir.core=> (count-members)
({:count 4})
kashmir.core=> (find-member-by-id {:id 2})
({:id 2, :first_name "Paul", :last_name "McCartney", :email "pmccartney@beatles.com", :created_at #inst "2015-10-14T19:59:48.905474000-00:00"})
kashmir.core=>
```

Fantastic! We can talk to the database through yesql based on the SQL scripts defined in "src/kashmir/sql/data.sql" file.
([Commit point](https://github.com/adomokos/kashmir/commit/fe6cba24ecaf163644a8ba7507cc74088ccd137a).)

#### Adding Tests

Although our application does not have much logic just yet, I'd like to show you how you could start writing automated tests. Create a new test file under "test/kashmir/data_test.clj". Add the following code to it:

```clojure
(ns kashmir.data-test
  (:require [clojure.test :refer :all]
            [kashmir.data :refer :all]))

(deftest count-members-test
  (testing "there are 4 members"
    (is (= 4 (-> (count-members) first :count)))))
```

Remove the failing test in "test/kashmir/core_test.clj" file:

```clojure
(ns kashmir.core-test
  (:require [clojure.test :refer :all]
            [kashmir.core :refer :all]))
```

Run the tests by invoking `lein test` and you should see the one and only test passing:

```shell
% lein test

lein test kashmir.data-test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

([Commit point](https://github.com/adomokos/kashmir/commit/0d8ac36f564c6894f91413d7e7295c09b8753642).)

#### Finding a Member

Yesql needs a hash even when a record is looked up by an ID. This is how you invoke the yesql generated function: `(find-member-by-id {:id 2})`. We should keep the data access interface unaware of this implementation detail. Let's find a member by an ID this way: `(find-member 2)`. Write the test for this in test/kashmir/data_test.clj:

```clojure
...

(deftest find-member-by-id-test
  (testing "finds PM with id 2"
    (is (= "Paul" (-> (find-member 2) first :first_name)))))
```

This is the code implementation of it in "src/kashmir/data.clj":

```clojure
...

(defn find-member [id]
  (find-member-by-id {:id id}))
```

Both of the tests should pass now.
([Commit point](https://github.com/adomokos/kashmir/commit/ca9dfd94daabb44b3a6777b978531ba405b1fd66).)

#### Adding a Member

Reading data with yesql is simple, but adding records and testing that over and over can be more challenging. The database has to be reset to its original state after each test run. You have two options here:

*   truncate all the tables after each test,
*   roll back the INSERT transactions.

The way you can truncate all the tables is a blog post in itself. Unfortunately the Clojure community has not created a [DatabaseCleaner](https://github.com/DatabaseCleaner/database_cleaner) project we love so much in the Ruby world just yet. Let's use the roll-back feature of the INSERT transaction for our tests in the examples.

When you create a member, you need to associate that member with a band. In fact, a member can not be added to the database without a band. A hash with all the member data and the band name will be the arguments to this create function.
Let's write the test first in the "test/kashmir/data_test.clj" file:

```clojure
...
(deftest create-member-test
  (testing "adds a member to the DB"
    (let [member {:first_name "Jim" :last_name "Morrison" :email "jmorrison@doors.com"}]
      (is (= 1 (create-member! member "The Doors"))))))
```

Let's write the simplest code that could possibly work. First, we need to add the INSERT SQL statements to "src/kashmir/data.sql". This is what those look like:

```sql
...

-- name: find-band-by-name
-- Finds a band record based on the provided name
SELECT *
FROM bands
WHERE name = :name

-- name: create-member-raw!
-- Adds a new member with the bands_members association
WITH inserted AS (
  INSERT INTO members (first_name, last_name, email)
  VALUES (:first_name, :last_name, :email)
  RETURNING id
)
INSERT INTO bands_members (member_id, band_id)
SELECT inserted.id, :band_id FROM inserted
```

As I was writing this blog post, I researched how I could insert records into different tables with one SQL statement. Using stored procedure or function would be an easy choice, but that's too heavy for what we need. I found [this blog post](http://rob.conery.io/2015/02/09/inserting-using-new-record-postgres/) by Rob Conery. He shows how CTEs (Common Table Expressions) can be used to insert and reuse the created record in a subsequent insert. That's what you see in the second SQL command. By using this solution, the Clojure code will be small, as the database write functionality is delegated to PostgreSQL.
This is what the data logic will look like in the "src/kashmir/data.clj" file:

```clojure
...

(defn create-member!
  ([member band-name]
    (let [band-id (-> (find-band-by-name {:name band-name})
                       first
                       :id)]
        (create-member-raw! (conj member {:band_id band-id})))))
```

The "-raw" postfix was used for the function that gets generated by yesql. We could have created an alias, but I liked this kind of naming-convention.
When you run the test it won't error out, but one of the tests will fail. It has more than 4 total records in the members table. Absolutely, the database was not restored to its default state. Let's take care of that! We will insert the record, but we will roll back the transaction once the test is complete, leaving the database in it's original, default state.
Add/modify the highlighted lines in your "src/kashmir/data.clj" file:

```clojure
...

(defn create-member!
  ([member band-name]
    (jdbc/with-db-transaction [tx db-spec]
      (create-member! member band-name tx)))
  ([member band-name tx]
    (let [band-id (-> (find-band-by-name {:name band-name})
                       first
                       :id)]
        (create-member-raw! (conj member {:band_id band-id})
                            {:connection tx}))))
```

And finally, initialize and roll back the transaction from the test. Change the highlighted lines in "test/kashmir/data_test.clj" this way:

```clojure
(ns kashmir.data-test
  (:require [clojure.test :refer :all]
            [kashmir.data :refer :all]
            [clojure.java.jdbc :as jdbc]))
  ...

(deftest create-member-test
  (jdbc/with-db-transaction [tx db-spec]
    (jdbc/db-set-rollback-only! tx)
      (testing "adds a member to the DB"
        (let [member {:first_name "Jim" :last_name "Morrison" :email "jmorrison@doors.com"}]
          (is (= 1 (create-member! member "The Doors" tx)))))))
```

Rebuild your database and run your tests. You should see the `0 failures, 0 errors.` message. Do it many times, the tests should always pass.
([Commit point](https://github.com/adomokos/kashmir/commit/5f4e280453dc96b0ca97e9774ef0d240c3b6872a).)

#### One Last Refactoring

I am unhappy with the `create-member!` function. The way we are looking up the band by its name is inelegant, I feel we could do better. Since we have one band record by name, when we call `find-band-by-name`, we should get back one single hash and not a lazy-seq with a hash in it. Let's refactor to that! First, we'll renamed the yesql generated function to `find-band-by-name-raw` in the "src/kashmir/sql/data.sql" file:

```sql
...

-- name: find-band-by-name-raw
-- Finds a band record based on the provided name
SELECT *
FROM bands
WHERE name = :name
```

Let's refactor the actual code like this in "src/kashmir/data.clj":

```clojure
...

(defn find-band-by-name [name]
  (first (find-band-by-name-raw {:name name})))

(defn create-member!
  ([member band-name]
    (jdbc/with-db-transaction [tx db-spec]
      (create-member! member band-name tx)))
  ([member band-name tx]
    (let [band-id (:id (find-band-by-name band-name))]
        (create-member-raw! (conj member {:band_id band-id})
                            {:connection tx}))))
```

I rebuilt the db, ran the tests and everything passed.
([Commit point](https://github.com/adomokos/kashmir/commit/4da1a44dd767e1afa44e38984b7e742dfb14c1b0).)

You could say this is only the "happy path", what if the band name is incorrect and no band will be found. This will blow up somewhere. Absolutely! You need to do exception handling and error checking. I wanted to keep my examples simple, so others coming to Clojure can benefit from the simplified code.

This last refactoring concludes the second part of the series. In the [final session](/blog/2015/11/clojure-api-with-yesql-migrations-part3.html) we will add logging to jdbc to monitor how yesql communicates with the database. We will also expose the data as JSON through an HTTP endpoint.


POSTED BY ATTILA DOMOKOS AT 10:38 AM


NO COMMENTS
