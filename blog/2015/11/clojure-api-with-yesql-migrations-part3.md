## Sunday, November 1, 2015

### [Clojure API with Yesql, Migrations and More (Part 3.)](http://www.adomokos.com/2015/11/clojure-api-with-yesql-migrations-part3.html)

We created a database with scripts, [added migrations](/blog/2015/10/clojure-api-with-yesql-migrations-and.md) and [communicated with the database](/blog/2015/10/clojure-api-with-yesql-migrations-part2.md) with the help of yesql in the previous posts. Please look at those first to get up to speed with this part.

In the final part of the series, we will serialize the data we pull from the database to JSON, and we will expose that data through an HTTP endpoint. We will also add logging to monitor the JDBC communication with the database.

It was about two years ago when I attended a conference and I sat down with a couple of friends one night for a chat. It was late in the evening, after a couple of beers they asked me what I was up to. I told them I am learning Clojure. They wanted to see it in action, we solved [FizzBuzz](http://www.adomokos.com/2013/12/test-driving-fizzbuzz-kata-in-clojure.html) together. They liked it, but one question was lingering there: "can you build a web app with Clojure?". Of course!
We started out as a console application, but the requirements have changed, we need to expose the data via an HTTP interface through JSON. I like to look at web frameworks as a "[delivery mechanism](https://www.youtube.com/watch?v=WpkDN78P884)", progressing the idea this way follows that.

Use this [commit](https://github.com/adomokos/kashmir/commit/4da1a44dd767e1afa44e38984b7e742dfb14c1b0) as a starting for this blog post. Rebuild the database by running `make build-db`.

#### Serializing the Data as JSON

We will use the [cheshire](https://github.com/dakrone/cheshire) library to serialize the data to JSON. Let's modify the "project.clj" file this way, see my changes highlighted:

```clojure
...
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.postgresql/postgresql "9.4-1201-jdbc41"]
                 [yesql "0.5.1"]
                 [cheshire "5.5.0"]]
...
```

The serialization should be taken care of by some kind of logic component. Let's write the test for this, place this content into your "test/kashmir/logic_test.clj" file:

```clojure
(ns kashmir.logic-test
  (:require [clojure.test :refer :all]
            [kashmir.logic :refer :all]
            [cheshire.core :as json]))

(deftest find-member-by-id-test
  (testing "returns a JSON serialized member record"
      (let [member (first (json/parse-string (find-member 2) true))]
        (is (= "Paul" (:first_name member))))))
```

Let's add the function skeleton to see test errors and not Java failures. Put this in the "src/kashmir/logic.clj" file:

```clojure
(ns kashmir.logic)

(defn find-member [id] nil)
```

Rebuild the database with the `make build-db` command. Running `lein test` should provide an output similar to this:

```shell
% lein test

lein test kashmir.data-test

lein test kashmir.logic-test

lein test :only kashmir.logic-test/find-member-by-id-test

FAIL in (find-member-by-id-test) (logic_test.clj:9)
returns a JSON serialized member record
expected: (= "Paul" (:first_name member))
  actual: (not (= "Paul" nil))

Ran 4 tests containing 4 assertions.
1 failures, 0 errors.
Tests failed.
```

Cheshire uses two main functions, `generate-string` to serialize and `parse-string` to deserialize data. We will have to serialize the data, please modify the "src/kashmir/logic.clj" file this way:

```clojure
(ns kashmir.logic
  (:require [kashmir.data :as data]
            [cheshire.core :as json]))

(defn find-member [id]
  (json/generate-string (data/find-member id)))
```

Run your tests again, all 4 should pass now.
As you think about, the logic namespace is responsible for making sure the data component returned data, handling exceptions and validating user input. This is the part of the app I'd test the most.
([Commit point](https://github.com/adomokos/kashmir/commit/d654cdc2f57bafd699fd65a9344805bd7ed506ec).)

#### Exposing the Data with Compojure

[Compojure](https://github.com/weavejester/compojure) is our go-to tool when it comes to building a web interface without much ceremony. Let's add it to our "project.clj" file:

```clojure
(defproject kashmir "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.postgresql/postgresql "9.4-1201-jdbc41"]
                 [yesql "0.5.1"]
                 [compojure "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [cheshire "5.5.0"]]
  :clj-sql-up {:database "jdbc:postgresql://kashmir_user:password@localhost:5432/kashmir"
               :deps [[org.postgresql/postgresql "9.4-1201-jdbc41"]]}
  :ring {:handler kashmir.handler/app}
  :plugins  [[clj-sql-up "0.3.7"]
             [lein-ring "0.9.7"]]
  :main ^:skip-aot kashmir.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring-mock "0.1.5"]]}})
```

We also need to add a "src/kashmir/handle.clj" file, that will handle the different web requests:

```clojure
(ns kashmir.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [kashmir.logic :as logic]))

(defroutes api-routes
    (GET "/" [] "Hello World")
    (GET "/members/:id{[0-9]+}" [id]
         {:status 200
          :headers {"Content-Type" "application/json; charset=utf-8"}
          :body (logic/find-member (read-string id))})
    (route/not-found "Not Found"))

(def app
    (wrap-defaults api-routes api-defaults))
```

Fire up the server with the `lein ring server-headless` command. Open up a new terminal window, and request the member with ID 2 using the curl command: `curl -i http://localhost:3000/members/2`. You should see something like this:

```shell
% curl -i http://localhost:3000/members/2
HTTP/1.1 200 OK
Date: Thu, 15 Oct 2015 17:31:44 GMT
Content-Type: application/json; charset=utf-8
Content-Length: 123
Server: Jetty(7.6.13.v20130916)

[{"id":2,"first_name":"Paul","last_name":"McCartney",
  "email":"pmccartney@beatles.com","created_at":"2015-10-15T16:50:03Z"}]%
```

The `-i` switch for curl will print out both the header and the body of the response.  
([Commit point](https://github.com/adomokos/kashmir/commit/8cc9b9ae500e0493a80866f24e06193fd25566f3).)

#### Using Ring Response

The way we are generating the response is too verbose, we are explicitly setting the status, the headers and the body. There are ring helpers we can take advantage of, making this a lot shorter.  
Change the "src/kashmir/handler.clj" file content to this (highlighted rows will designate changes):

```clojure
(ns kashmir.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.util.response :as rr]
            [kashmir.logic :as logic]))

(defroutes api-routes
    (GET "/" [] "Hello World")
    (GET "/members/:id{[0-9]+}" [id]
         (rr/response (logic/find-member (read-string id))))
    (route/not-found "Not Found"))

(def app
    (wrap-defaults api-routes api-defaults))
```

Fire up the server, run the curl request, everything should still work the same.
([Commit point](https://github.com/adomokos/kashmir/commit/c9e9bb433d336a31ef3a9c00eaa89dc9b297fda9).)

#### Stubbing out Data Access in Logic Tests

Hitting the database for the logic function is feasible, but it won't buy you all that much. You can stub out your database call with Clojure's `with-redefs` function. You need to define a function that returns the value the data access function would return.

Modify the "test/kashmir/logic_test.clj" file this way:

```clojure
(ns kashmir.logic-test
  (:require [clojure.test :refer :all]
            [kashmir.logic :refer :all]
            [kashmir.data :as data]
            [cheshire.core :as json]))

(deftest find-member-by-id-test
  (testing "returns a JSON serialized member record"
    (with-redefs [data/find-member (fn [id] [{:first_name "Paul"}])]
      (let [member (first (json/parse-string (find-member 2) true))]
        (is (= "Paul" (:first_name member)))))))
```

Now, stop your Postgres database server and run this test, it should pass as it's not hitting the database, it purely tests the hash serialization.
([Commit point](https://github.com/adomokos/kashmir/commit/debc055da395f3d18edad13fd9d69b0cdeb0cfe8).)

#### Adding JDBC Logging

Our solution works well as it is, however, we don't see what kind of SQL statements are executed against the database. Turning on logging in Postgres is one option, but monitoring JDBC within our application is prefereable. We will use the [log4jdbc](https://github.com/arthurblake/log4jdbc) library to log jdbc activities. This library is using the [Simple Logging Facade For Java](http://www.slf4j.org/download.html) library, you need to add that jar file to the project.

Download the [slf4j jar file](http://www.slf4j.org/dist/slf4j-1.7.12.tar.gz) and add it to the project's lib directory. Then modify the "project.clj" file this way:

```clojure
                  [yesql "0.5.1"]
                  [compojure "1.4.0"]
                  [ring/ring-defaults "0.1.5"]
                  [cheshire "5.5.0"]]
                  [cheshire "5.5.0"]
                  [com.googlecode.log4jdbc/log4jdbc "1.2"]]
   :clj-sql-up {:database "jdbc:postgresql://kashmir_user:password@localhost:5432/kashmir"
                :deps [[org.postgresql/postgresql "9.4-1201-jdbc41"]]}
   :ring {:handler kashmir.handler/app}
   :resource-paths ["lib/slf4j-simple-1.7.12.jar"]
   :plugins  [[clj-sql-up "0.3.7"]
              [lein-ring "0.9.7"]]
   :main ^:skip-aot kashmir.core
```

You need to configure slf4j, you can do that by adding this content to the "resources/log4j.properties" file:

```
# the appender used for the JDBC API layer call logging above, sql only
log4j.appender.sql=org.apache.log4j.ConsoleAppender
log4j.appender.sql.Target=System.out
log4j.appender.sql.layout=org.apache.log4j.PatternLayout
log4j.appender.sql.layout.ConversionPattern= \u001b[0;31m (SQL)\u001b[m %d{yyyy-MM-dd HH:mm:ss.SSS} \u001b[0;32m %m \u001b[m %n

# ==============================================================================
# JDBC API layer call logging :
# INFO shows logging, DEBUG also shows where in code the jdbc calls were made,
# setting DEBUG to true might cause minor slow-down in some environments.
# If you experience too much slowness, use INFO instead.

log4jdbc.drivers=org.postgresql.Driver

# Log all JDBC calls except for ResultSet calls
log4j.logger.jdbc.audit=FATAL,sql
log4j.additivity.jdbc.audit=false

# Log only JDBC calls to ResultSet objects
log4j.logger.jdbc.resultset=FATAL,sql
log4j.additivity.jdbc.resultset=false

# Log only the SQL that is executed.
log4j.logger.jdbc.sqlonly=FATAL,sql
log4j.additivity.jdbc.sqlonly=false

# Log timing information about the SQL that is executed.
log4j.logger.jdbc.sqltiming=FATAL,sql
log4j.additivity.jdbc.sqltiming=false

# Log connection open/close events and connection number dump
log4j.logger.jdbc.connection=FATAL,sql
log4j.additivity.jdbc.connection=false
```

Finally, you need to modify the "src/kashmir/data.clj" file to use the logger Postgres connection:

```clojure
(:require [yesql.core :refer [defqueries]]
             [clojure.java.jdbc :as jdbc]))

 (def db-spec {:classname "net.sf.log4jdbc.DriverSpy"
               :subprotocol "log4jdbc:postgresql"
               :subname "//localhost:5432/kashmir"
               :user "kashmir_user"
               :password "password1"})
```

Now when you run the tests or hit the HTTP endpoint with cURL, you should see the JDBC logs in the terminal:

```shell
% lein test kashmir.data-test
[main] INFO jdbc.connection - 1\. Connection opened
[main] INFO jdbc.audit - 1\. Connection.new Connection returned
[main] INFO jdbc.audit - 1\. PreparedStatement.new PreparedStatement returned
[main] INFO jdbc.audit - 1\. Connection.prepareStatement(SELECT *
FROM members
WHERE id = ?) returned net.sf.log4jdbc.PreparedStatementSpy@51dbed72
[main] INFO jdbc.audit - 1\. PreparedStatement.setObject(1, 2) returned
[main] INFO jdbc.sqlonly - SELECT * FROM members WHERE id = 2
...
```

([Commit point](https://github.com/adomokos/kashmir/commit/81d40aa906908e815f16f53e00701124a6701405).)

As you can see, the log can be verbose. The easiest way I found to turn off logging is changing the `log4jdbc:postgresql` subprotocol back to the original value: `postgresql`.
([Commit point](https://github.com/adomokos/kashmir/commit/2300a4a90caf3599fb2c23337a3b60dcda931305).)

This last step concludes the series. We set up a database build process, added migrations and seed data to it. We separated SQL from Clojure by using the yesql library. We added testing with mocking to make sure our code is working properly. We exposed the data as JSON through an HTTP endpoint and we added JDBC logging to the project to monitor the communication with the database.

I hope you will find this exercise helpful. Good luck building your database backed Clojure solution!


POSTED BY ATTILA DOMOKOS AT 8:13 PM


#### 1 comment:

[Unknown](https://www.blogger.com/profile/12030207078405578691) said...

_Hi Attila! I just wanted to say thank you for this concise and informative tutorial. I am starting to work on my first REST API with Clojure and your tutorial helped me to answer a lot of questions!! Keep the great work!_

November 27, 2015 at 10:35 PM
