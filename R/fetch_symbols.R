#' Fetch S&P 500 symbols from the database
#'
#' This function retrieves a list of distinct symbols and their corresponding index_ts
#' from the `sp500.info` table in the PostgreSQL database.
#'
#' @param con A valid DBI database connection.
#'
#' @return A tibble containing two columns: `symbol` and `index_ts`.
#' If no symbols are found, a warning is issued and an empty tibble is returned.
#' @export
fetch_symbols <- function(con) {

  if (is.null(con)) {
    stop("Parameter 'con' must be provided.")
  }

  query <- glue::glue_sql(
    "SELECT DISTINCT symbol, index_ts FROM sp500.info",
    .con = con
  )

  result <- DBI::dbGetQuery(con, query)

  if (nrow(result) == 0) {
    warning("No symbols found in the database.")
  }

  tibble::as_tibble(result)
}
