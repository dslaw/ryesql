DBI_FN_MAP <- list(
    reads = list(
        params = "dbGetPreparedQuery",
        none = "dbGetQuery"
    ),
    modifies = list(
        params = "dbGetPreparedQuery",
        none = "dbExecute"
    )
)

#' Create a callable query object.
#'
#' Convert a named list, as returned by `parse_named_query`,
#' into a function with attached metadata.
#'
#' A package defining DBI methods must be loaded prior to
#' calling this function.
#' 
#' @param query List.
#' @return Function.
mk_callable <- function(query) {
    if (!(query$type %in% names(DBI_FN_MAP))) {
        warning(paste0("'", query$type, "'",
                       " is not supported.\n",
                       "Use `get_query` or `get_queries`."),
                call. = FALSE)
    }

    # Dynamically look up method defined by DBI.
    param <- if (query$prepared) "params" else "none"
    fn_name <- DBI_FN_MAP[[query$type]][[param]]
    on.exit(rm(param, fn_name))
    fn <- match.fun(fn_name)

    callable <- function(conn, ...) {
        fn(conn = conn, statement = query$sql, ...)
    }

    # Set metadata on inner function.
    # `attr` only takes character strings - skip boolean field.
    query$prepared <- NULL
    for (field in names(query)) {
        attr(callable, field) <- query[[field]]
    }

    class(callable) <- c("yesql", class(callable))
    callable
}

#' Get queries.
#'
#' Create named-queries from a sql file. Each query is
#' returned as a named list with a name, description and
#' SQL string suitable for passing to a DBIConnection
#' method.
#'
#' @param filename Character string.
#' @return List of named-queries.
#' @export
get_queries <- function(filename) {
    txt <- readLines(filename)
    query_blocks <- split_queries(txt)
    queries <- lapply(query_blocks, parse_named_query)
    names(queries) <- lapply(queries, function(x) x$name)
    queries
}

#' Get query.
#'
#' Create an anonymous query from a sql file. The
#' query is returned as a named list with a description
#' and SQL string suitable for passing to a DBIConnection
#' method.
#'
#' The file should contain only a single query to be
#' loaded.
#'
#' @param filename Character string.
#' @return Named list.
#' @export
get_query <- function(filename) {
    txt <- readLines(filename)
    parse_anon_query(txt)
}

#' Load queries into an environment.
#'
#' Create query functions inside an environment. Multiple
#' queries are added to the specified environment, identified
#' by their names.
#'
#' @param filename Character string.
#' @param env Environment to load the queries into. Default
#' is the calling environment.
#' @return NULL
#' @seealso get_queries
#' @export
load_queries <- function(filename, env = parent.frame()) {
    queries <- lapply(get_queries(filename), mk_callable)

    for (query in queries) {
        name <- attr(query, "name")
        env[[name]] <- query
    }

    invisible(NULL)
}

#' Load query into an environment.
#'
#' Create a query function inside an environment. The query
#' is added to the specified environment as the given name.
#'
#' @param name Character string. Name to be given to the query.
#' @param filename Character string.
#' @param env Environment to load the queries into. Default
#' is the calling environment.
#' @return NULL
#' @seealso get_query
#' @export
load_query <- function(name, filename, env = parent.frame()) {
    query_obj <- get_query(filename)

    # Set metadata as necessary for `mk_callable`.
    query_obj$name <- name
    query_obj$type <- "modifies" # Default to the most general case.
    query <- mk_callable(query_obj)

    env[[name]] <- query
    invisible(NULL)
}

#' @export
print.yesql <- function(x, ...) {
    call_signature <- paste0("`", attr(x, "name"), "`", "(conn, ...)")

    description <- attr(x, "description")
    if (!is.character(description)) {
        description <- "No description."
    }

    repr <- paste0(c(call_signature, "\n", description), collapse = "\n")
    cat(repr, "\n")

    invisible(x)
}

#' Return the raw SQL.
#'
#' Get the SQL stored in the query closure.
#'
#' @param query Function.
#' @return Character string.
#'
#' @examples
#' \dontrun{
#' require(RSQLite)
#'
#' writeLines("SELECT DISTINCT color FROM fruits;", con = "query.sql")
#' load_query("get_colors", "query.sql")
#' get_sql(get_colors)
#' # [1] "SELECT DISTINCT color FROM fruits"
#' }
#'
#' @export
get_sql <- function(query) {
    attr(query, "sql")
}

