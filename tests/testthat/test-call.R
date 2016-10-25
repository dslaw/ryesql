library(ryesql)
library(RSQLite)

context("Queries integrate with DBIConnection methods")


QUERIES_FILE <- "data/queries.sql"

setup <- function(conn) {
    RSQLite::dbWriteTable(conn, "fruits",
                 data.frame(name = c("apple", "pear", "strawberry", "orange"),
                            color = c("green", "green", "red", "orange")))
}


test_that("mk_callable returns correct type", {
    mock_query <- list(name = "mock-query",
                       description = "Shoplifters of the world",
                       sql = "SELECT * FROM t",
                       prepared = FALSE,
                       type = "reads")
    out <- mk_callable(mock_query)

    expect_is(out, "yesql")
    expect_is(out, "function")

    # Preserves metadata.
    expect_equal(attr(out, "name"), mock_query$name)
    expect_equal(attr(out, "description"), mock_query$description)
    expect_equal(attr(out, "sql"), mock_query$sql)
    expect_equal(attr(out, "type"), mock_query$type)
})

test_that("query function selects", {
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(RSQLite::dbDisconnect(conn))
    setup(conn)

    expected <- c("apple", "pear", "strawberry", "orange")
    e <- new.env()
    load_queries(QUERIES_FILE, env = e)

    out <- e$`get-fruits`(conn)
    expect_equal(out[["name"]], expected)
})

test_that("query function modifies", {
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(RSQLite::dbDisconnect(conn))
    setup(conn)

    e <- new.env()
    load_queries(QUERIES_FILE, env = e)

    # Named parameters.
    expected_mango <- "orange"
    e$`add-fruit!`(conn, data.frame(fruit = "mango", color = expected_mango))
    out <- dbGetQuery(conn, "SELECT color FROM fruits WHERE name = 'mango'")
    expect_equal(out[["color"]], expected_mango)

    # Positional parameter.
    expected_orange <- "moldy-green"
    e$`update-orange!`(conn, data.frame(new_color = expected_orange))
    out <- dbGetQuery(conn, "SELECT color FROM fruits WHERE name = 'orange'")
    expect_equal(out[["color"]], expected_orange)
})

