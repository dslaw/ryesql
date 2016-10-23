library(ryesql)
context("Parse SQL files")


test_that("split_queries splits named-queries", {
    txt <- c(
        "-- File description. ",
        "",
        "-- name: my-query",
        "-- Queries for foo.",
        "SELECT foo",
        "FROM tbl",
        "GROUP BY bar;",
        "",
        "-- name: update!",
        "-- Updates foo.",
        "UPDATE tbl",
        "SET foo=1",
        "WHERE bar < 10;",
        "")
    expected <- list(
        c("-- name: my-query",
          "-- Queries for foo.",
          "SELECT foo",
          "FROM tbl",
          "GROUP BY bar;"),
        c("-- name: update!",
          "-- Updates foo.",
          "UPDATE tbl",
          "SET foo=1",
          "WHERE bar < 10;"))

    queries <- split_queries(txt)
    expect_equal(queries, expected)
})

test_that("split_queries throws for invalid queries", {
    txt <- c("-- name: get-foos",
             "-- Gets the foos.",
             "",
             "-- name: get-ids",
             "-- Gets ids.",
             "SELECT id",
             "FROM tbl;")
    expect_error(split_queries(txt), "Invalid syntax")
})

test_that("is_name identifies valid names", {
    expect_true( is_name("-- name: foo-bar") )

    # Modifying query.
    expect_true( is_name("-- name: foo-bar!") )

    # Returning query.
    expect_true( is_name("-- name: foo-bar<!") )

    # Leading/trailing whitespace.
    expect_true( is_name(" -- name: foo-bar ") )

    # Only check for name identifier - don't validate names.
    expect_true( is_name("-- name: foo bar") )
})

test_that("is_name identifies non-names", {
    expect_false( is_name("-- foo-bar") )
    expect_false( is_name("/* name: foo-bar */") )
})

test_that("extract_name gets name", {
    expected <- "foo-bar"

    out <- extract_name("-- name: foo-bar")
    expect_equal(out, expected)

    # Modifying query.
    out <- extract_name("-- name: foo-bar!")
    expect_equal(out, "foo-bar!")

    # Returning query.
    out <- extract_name("-- name: foo-bar<!")
    expect_equal(out, "foo-bar<!")

    # Leading/trailing whitespace.
    out <- extract_name(" --name: foo-bar ")
    expect_equal(out, expected)

    # No space between name and name-identifier.
    out <- extract_name("--name:foo-bar")
    expect_equal(out, expected)
})

test_that("extract_name throws for invalid name", {
    expected_msg_partial <- "does not contain a valid name"

    expect_error(extract_name("-- name: foo bar"), expected_msg_partial)
    expect_error(extract_name("-- name: foo, bar"), expected_msg_partial)
    expect_error(extract_name("-- name: foo (does bar)"), expected_msg_partial)
    expect_error(extract_name("-- name: foo!!bar"), expected_msg_partial)
})

test_that("extract_description gets description", {
    txt <- c("-- Gets the foos.",
             "--   bar : filter by",
             "SELECT foo",
             "FROM tbl",
             "WHERE id = :bar;")
    expected <- c("Gets the foos.",
                  "  bar : filter by")
    expect_equal(extract_description(txt), expected)

    # No description.
    txt <- c("SELECT foo",
             "FROM tbl",
             "WHERE id = :bar;")
    expect_equal(extract_description(txt), NA)
})

test_that("extract_sql gets sql", {
    txt <- c("-- Gets the foos.",
             "--   bar : filter by",
             "SELECT foo",
             "FROM tbl",
             "WHERE id = :bar;")
    expected <- "SELECT foo\nFROM tbl\nWHERE id = :bar;"
    expect_equal(extract_sql(txt), expected)
})

test_that("query_type identifies query from name", {
    expect_equal(query_type("modify!"), "modifies")
    expect_equal(query_type("get-results<!"), "returns")
    expect_equal(query_type("read-tbl"), "reads")
})

test_that("is_prepared identifies parameterized queries", {
    # Named parameter.
    sql <- c("SELECT *",
             "FROM tbl",
             "WHERE id > :i;")
    expect_true(is_prepared(sql))

    # Snake case.
    sql <- c("SELECT *",
             "FROM tbl",
             "WHERE id > :foo_bar;")
    expect_true(is_prepared(sql))

    # Anonymous parameter/placeholder.
    sql <- c("SELECT *",
             "FROM tbl",
             "WHERE id > ?;")
    expect_true(is_prepared(sql))

    # No parameter.
    sql <- c("SELECT *",
             "FROM tbl",
             "WHERE id > 4;")
    expect_false(is_prepared(sql))

    # Parameter in comment.
    sql <- c("SELECT *",
             "FROM tbl",
             "-- should use placeholder eg ?",
             "WHERE id > 10;")
    expect_false(is_prepared(sql))

    sql <- c("SELECT *",
             "FROM tbl",
             "WHERE id > 10; /* ignore :i */")
    expect_false(is_prepared(sql))
})

test_that("parse_named_query parses named-query", {
    txt <- c("-- name: add-event!",
             "-- Add an event.",
             "INSERT INTO events",
             "VALUES (?, ?, ?);")
    out <- parse_named_query(txt)

    expect_equal(class(out), c("query", "named", "list"))
    expect_equal(out$name, "add-event!")
    expect_equal(out$type, "modifies")
    expect_equal(out$description, "Add an event.")
    expect_equal(out$sql, "INSERT INTO events\nVALUES (?, ?, ?)") # no semicolon
    expect_true(out$prepared)
})

test_that("parse_anon_query parses query", {
    txt <- c("-- Add an event.",
             "INSERT INTO events",
             "VALUES (?, ?, ?);")
    out <- parse_anon_query(txt)

    expect_equal(class(out), c("query", "list"))
    expect_equal(out$description, "Add an event.")
    expect_equal(out$sql, "INSERT INTO events\nVALUES (?, ?, ?)") # no semicolon
    expect_true(out$prepared)
})

