# https://github.com/krisajenkins/yesql/blob/master/src/yesql/queryfile.bnf


comment_ <- stringr::regex("^\\s*--")
name_tag <- stringr::fixed("name:")
delimiter <- ";"
placeholder <- stringr::regex("\\?")
parameter <- stringr::regex(":\\w+")


syntax_error <- function(msg = "Invalid syntax") {
    stop(msg, call. = FALSE)
}

rm_pattern <- function(string, pattern) {
    stringr::str_replace(string, pattern, "")
}

not_blank <- function(string) {
    stringr::str_detect(string, stringr::boundary("word"))
}

#' Split slurped SQL into named-queries.
#'
#' @param lines Character vector of SQL statements to split.
#' @return queries List of queries.
split_queries <- function(lines) {
    trimmed <- Filter(not_blank, lines)
    line_comment <- stringr::str_detect(trimmed, comment_)
    name <- stringr::str_detect(trimmed, name_tag)
    delim <- stringr::str_detect(trimmed, delimiter)

    begin_indexes <- which(line_comment & name)
    end_indexes <- which(!line_comment & delim)

    # Check every name has a query.
    if (length(begin_indexes) != length(end_indexes) ||
        any((end_indexes - begin_indexes) < 1L)) {
        syntax_error()
    }

    mapply(function(i, j) trimmed[i:j],
           begin_indexes,
           end_indexes,
           SIMPLIFY = FALSE)
}

#' Check if the line contains a query name.
is_name <- function(string) {
    name_pattern <- stringr::str_c(comment_, "\\s*", name_tag, ".+?")
    stringr::str_detect(string, stringr::regex(name_pattern))
}

#' Get query name.
#'
#' Extract the name of the query from the docstring.
#' @param string Character vector giving the query.
#' @return query_name Character string.
extract_name <- function(string) {
    # Input should be validated as a name line prior to passing in.
    pattern <- stringr::str_c("^.*", name_tag)
    query_name <- rm_pattern(string, pattern)

    # Validate remaining text.
    # Replace dashes with underscores so it counts as a single word.
    snake_case <- stringr::str_replace_all(query_name, "-", "_")
    n_words <- stringr::str_count(snake_case, stringr::boundary("word"))

    if (n_words != 1L) {
        msg <- paste0("'", string, "'", " does not contain a valid name")
        syntax_error(msg)
    }

    stringr::str_trim(query_name)
}

#' Get query description.
#'
#' Extract the query description from the docstring.
#' @param lines Character vector giving the query docstring.
#' @return description Character vector.
extract_description <- function(lines) {
    comment_pattern <- stringr::str_c(comment_, "[ ]")

    docstring <- stringr::str_subset(lines, comment_)
    description <- rm_pattern(docstring, stringr::regex(comment_pattern))
    if (length(description)) description else NA
}

#' Get SQL query.
#'
#' Extract the SQL query.
#' @param lines Character vector giving the full query.
#' @return sql Character string.
extract_sql <- function(lines) {
    is_query <- !stringr::str_detect(lines, comment_)
    sql <- lines[is_query]
    # Queries using DBI must be a length one char vector.
    stringr::str_c(sql, collapse = "\n")
}

#' Determine the type of query.
#'
#' Check the query name for identifying information.
#' @param query_name Character string. The parsed query name.
#' @return type Character string.
query_type <- function(query_name) {
    # 'reads' will always match (default case).
    types <- c(returns = "<!", modifies = "!", reads = ".*")
    matches <- stringr::str_detect(query_name, types)
    matched_types <- names(types)[matches]
    matched_types[1L]
}

#' Check if the query contains parameters.
is_prepared <- function(sql) {
    # Remove comments to avoid false positives.
    line_comment <- stringr::regex("--.*$")
    block_comment <- stringr::regex("/\\*.*\\*/", multiline = TRUE)
    # Vectorize over `sql`.
    stripped <- rm_pattern(rm_pattern(sql, block_comment), line_comment)

    detected_params <- stringr::str_detect(stripped, parameter)
    detected_placeholders <- stringr::str_detect(stripped, placeholder)
    any(c(detected_params, detected_placeholders))
}

#' Parse a docstring with name and sql block.
parse_named_query <- function(block) {
    first <- head(block, n = 1L)
    rest <- tail(block, n = -1L)

    if (!is_name(first)) {
        msg <- paste0("Query must have a ", "'", name_tag, "'", " tag in the first line")
        syntax_error(msg)
    }

    query <- list()
    class(query) <- c("query", "named", class(query))

    sql <- extract_sql(rest)
    query$name <- extract_name(first)
    query$type <- query_type(query$name)
    query$description <- extract_description(rest)
    query$sql <- rm_pattern(sql, delimiter)
    query$prepared <- is_prepared(sql)
    query
}

#' Parse a docstring and sql block.
parse_anon_query <- function(block) {
    query <- list()
    class(query) <- c("query", class(query))

    sql <- extract_sql(block)
    query$description <- extract_description(block)
    query$sql <- rm_pattern(sql, delimiter)
    query$prepared <- is_prepared(sql)
    query
}
