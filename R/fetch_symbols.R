#' Fetch Stock Symbols from Database Table
#'
#' @description Retrieves stock ticker symbols from a specified table and column
#'   in the connected PostgreSQL database. Assumes the table contains the desired
#'   list of symbols (e.g., S&P 500 constituents).
#'
#' @param db_connection A valid DBIConnection object to your PostgreSQL database
#'   (e.g., returned by `connect_db()`).
#' @param table_name Character string. The name of the table in the database
#'   containing the symbols. Defaults to "sp500".
#' @param symbol_col_name Character string. The name of the column within
#'   `table_name` that holds the ticker symbols. Defaults to "symbol".
#'
#' @return A character vector of stock symbols retrieved from the database.
#'   Returns an empty character vector if the query fails or the table/column
#'   is empty or not found (issues warnings).
#'
#' @importFrom DBI dbIsValid dbQuoteIdentifier dbGetQuery
#' @importFrom glue glue
#' @importFrom rlang abort warn inform
#' @export
#' @examples
#' \dontrun{
#' # --- Assumes ---
#' # con <- connect_db() # Established connection
#' # A table named 'sp500' exists with a column named 'symbol'
#'
#' # --- Example Usage ---
#' symbols_from_db <- fetch_symbols(db_connection = con)
#' head(symbols_from_db)
#'
#' # Example specifying table and column names
#' symbols_custom <- fetch_symbols(db_connection = con,
#'                                 table_name = "my_ticker_list",
#'                                 symbol_col_name = "ticker")
#' head(symbols_custom)
#'
#' DBI::dbDisconnect(con)
#' }
fetch_symbols <- function(db_connection, table_name = "sp500", symbol_col_name = "symbol") {

  # --- Basic Input Validation ---
  if (!DBI::dbIsValid(db_connection)) {
    rlang::abort("Invalid database connection provided.")
  }
  if (!is.character(table_name) || length(table_name) != 1 || nchar(table_name) == 0) {
    rlang::abort("'table_name' must be a non-empty character string.")
  }
  if (!is.character(symbol_col_name) || length(symbol_col_name) != 1 || nchar(symbol_col_name) == 0) {
    rlang::abort("'symbol_col_name' must be a non-empty character string.")
  }
  # Check for glue package dependency (optional but good if used)
  if (!requireNamespace("glue", quietly = TRUE)) {
    # Alternative: use paste0 if glue is not mandatory
    # message("Consider installing 'glue' package for cleaner SQL construction.")
    rlang::abort("Package 'glue' is recommended for this function (SQL construction).")
  }

  # --- Safely Quote Identifiers ---
  safe_table <- DBI::dbQuoteIdentifier(db_connection, table_name)
  safe_col <- DBI::dbQuoteIdentifier(db_connection, symbol_col_name)

  # --- Construct SQL Query ---
  # Selecting only the specified column
  sql <- glue::glue("SELECT {safe_col} FROM {safe_table}")

  # --- Execute Query ---
  rlang::inform(paste("Querying database for symbols from table:", table_name, "column:", symbol_col_name))
  result_df <- tryCatch({
    DBI::dbGetQuery(db_connection, sql)
  }, error = function(e) {
    rlang::warn(paste("Failed to query table", table_name, "for symbols:", e$message))
    return(NULL) # Signal error
  })

  # --- Process Results ---
  if (is.null(result_df)) {
    # Error occurred during query
    return(character(0))
  } else if (nrow(result_df) == 0) {
    # Query succeeded but returned no rows
    rlang::warn(paste("No symbols found in table '", table_name, "' column '", symbol_col_name, "'.", sep=""))
    return(character(0))
  } else if (!symbol_col_name %in% names(result_df)) {
    # Safety check: Query succeeded but expected column is missing in result
    rlang::warn(paste("Column '", symbol_col_name, "' not found in the result from table '", table_name, "'. Check column name.", sep=""))
    return(character(0))
  }
  else {
    # Success: Extract the column as a vector
    # Use [[ ]] to access the column by its string name
    symbols_vector <- result_df[[symbol_col_name]]
    rlang::inform(paste("Successfully fetched", length(symbols_vector), "symbols from database table:", table_name))
    return(symbols_vector)
  }
}
