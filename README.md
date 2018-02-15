# ryesql

An R package for using SQL based on the
[Yesql](https://github.com/krisajenkins/yesql) Clojure library.


## Usage

`ryesql` handles reading SQL queries from a file and executing them against a
DBI connection.

There are two methods for reading queries: named queries, which have metadata,
and anonymous queries, which do not have metadata. Multiple named queries can
defined and loaded from a single file, while anonymous queries must each inhabit
their own file.

Named queries can be loaded with the function `load_queries`. Given the file
`fruit.sql`, with the contents:

```sql
-- name: green-fruit
-- Get all green fruit.
SELECT name
FROM fruits
WHERE color = ?
```

the query `green-fruit` can be loaded into the global environment:

```r
require(RSQLite)  # Or another DBI package.

load_queries("fruit.sql")

print(`green-fruit`)
# `green-fruit`(conn, ...)
#
#
# Get all green fruit.

get_sql(`green-fruit`)
# [1] "SELECT name\nFROM fruits\nWHERE color = ?"
```

The loaded query is a function which takes a DBIConnection:

```r
conn <- dbConnect(SQLite(), ":memory:")
values <- data.frame(name = c("apple", "orange", "watermelon"),
                     color = c("green", "orange", "green"))
dbWriteTable(conn, "fruits", values)

params <- data.frame(color = "green")
`green-fruit`(conn, bind.data = params)
#         name
# 1      apple
# 2 watermelon
```

To load an anonymous query, a name must be provided:

```r
load_query("my-query", "anonymous.sql")
```

Note that in both cases, database interface functions must be loaded before
loading any queries (i.e. load the database package first).

By default, queries are loaded into the global environment. This behavior can be
overridden by passing another environment to the loading function:

```r
queries <- new.env()
load_queries("fruit.sql", env = queries)

print(queries$`green-fruit`)
```
