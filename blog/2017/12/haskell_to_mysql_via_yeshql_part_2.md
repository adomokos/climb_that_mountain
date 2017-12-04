### Haskell to MySQL via Yeshql (Part 2.)

In the [previous](blog/2017/11/haskell_to_mysql_via_yeshql.md) blog post we built the skeleton of a console app in Haskell that talks to MySQL via Yeshql. We used a Makefile to rebuild the database, compile the code and run the app. [This is](https://github.com/adomokos/hashmir/commit/78a597e2c348abe751178812367f260fde69edb6) the commit point where we left it at the end of Part 1.

#### Insert a Client record

We can query the `clients` table, however, there are no records in that table. Let's add one with a SQL template.

Modify the yeshql templates by adding the following code:

```haskell
 [yesh|
     -- name:countClientSQL :: (Int)
     SELECT count(id) FROM clients;
     ;;;
     -- name:insertClientSQL
     -- :client_name :: String
     -- :subdomain :: String
     INSERT INTO clients (name, subdomain) VALUES (:client_name, :subdomain);
 |]
```

Three semicolons are used to separate SQL statements. The `-- name` is used to provide a function name that we can refer to in our code. The two lines are the input arguments to the generated function. Those arguments are the values in the INSERT statement on the last line.

Let's add the following Haskell function to insert a new Client record:

```haskell
insertClient :: String -> String -> IO ()
insertClient name subdomain = do
    conn <- getConn
    clientId <- insertClientSQL name subdomain conn
    commit conn
    disconnect conn
    putStrLn $ "New client's id is " ++ show clientId
```

The generated `insertClientSQL` (note the name in the SQL template) is invoked with the two specified argument plus the connection. Uncommitted connection will not write the record to the table, without that statement the `countClientSQL` function would return 0 records.
Disconnecting a connection is a good practice (if you can't pool those connections), free up resources when you don't need them.

Invoke the created `insertClient` function from `main` like this:

```haskell
main = do
    insertClient "TestClient" "testclient"
    countClient
```

When I try to build the app (use `make build` for it), I get the following errors:

```shell
/Haskell/hashmir/app/Main.hs:34:5: error:
    Variable not in scope: commit :: Connection -> IO a0
```

It turns out that `Database.HDBC` library is needed to invoke `commit` and `disconnect`.
Add the following import statement to the top of the file:

```haskell
import qualified Database.HDBC as H
```

Update the function to use the qualified names for the `commit` and `disconnect` calls.

```haskell
insertClient :: String -> String -> IO ()
insertClient name subdomain = do
    conn <- getConn
    clientId <- insertClientSQL name subdomain conn
    H.commit conn
    H.disconnect conn
    putStrLn $ "New client's id is " ++ show clientId
```

The project should successfully build, when you run the app (use `make run` to do it), you should see this output:

```shell
% make run
Dropping and rebuilding database hashmir_test
time ~/.local/bin/hashmir-exe
New client's id is 1
There are 1 records.

real	0m0.020s
user	0m0.009s
sys	0m0.006s
```

The database got rebuilt, one client record was inserted and the count function counted that record.

[Commit point](https://github.com/adomokos/hashmir/commit/dbdbcc3d41bf84f7230ef0ff98f78f6752c115ab)
