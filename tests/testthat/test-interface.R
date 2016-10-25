library(ryesql)
context("Load queries from a file")


QUERIES_FILE <- "data/queries.sql"
QUERY_FILE <- "data/query.sql"


test_that("get_queries returns correct queries", {
    expected <- c(
        "get-fruits" = "SELECT name\nFROM fruits",
        "add-fruit!" = "INSERT INTO fruits (name, color)\nVALUES (:fruit, :color)",
        "update-orange!" = "UPDATE fruits\nSET color = ?\nWHERE name = 'orange'")
    out <- get_queries(QUERIES_FILE)

    expect_is(out, "list")
    expect_equal(names(out), names(expected))

    expect_equal(out[[1L]]$sql, expected[[1L]])
    expect_equal(out[[2L]]$sql, expected[[2L]])
    expect_equal(out[[3L]]$sql, expected[[3L]])
})

test_that("get_queries returns empty if no named-queries found", {
    # Empty named list.
    expected <- list()
    names(expected) <- list()

    out <- get_queries(QUERY_FILE)
    expect_equal(out, expected)
})

test_that("get_query returns a query", {
    # Preserve indentation.
    expected <- c("CREATE TABLE fruits (",
                  "    id INTEGER PRIMARY KEY,",
                  "    name VARCHAR(32) NOT NULL,",
                  "    color VARCHAR(32)",
                  ")")
    expected <- paste0(expected, collapse = "\n")
    out <- get_query(QUERY_FILE)

    expect_is(out, "list")
    expect_equal(out$sql, expected)
})

test_that("load_queries sets queries", {
    expected_names <- c("get-fruits", "add-fruit!", "update-orange!")
    e <- new.env()
    load_queries(QUERIES_FILE, env = e)

    difference <- setdiff(names(e), expected_names)
    expect_equal(length(difference), 0L)
})

test_that("load_query sets query", {
    name <- "create-fruits"
    e <- new.env()
    load_query(name, QUERY_FILE, env = e)

    expect_true(name %in% names(e))
})

