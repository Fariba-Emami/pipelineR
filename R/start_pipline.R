#' Start Data Pipeline using Yahoo Finance Data
#'
#' @description Orchestrates a pipeline to fetch data for multiple symbols
#'   from Yahoo Finance, format it, and insert it into a database table.
#'
#' @param symbols_to_process A character vector of stock symbols.
#' @param db_connection A valid DBI connection object.
#' @param table_name The name of the target database table.
#' @param date_from Character string or Date. Start date for the data query.
#' @param date_to Character string or Date. End date for the data query. Defaults to `Sys.Date()`.
#' @param format_digits Integer. Digits for rounding in `format_data`. Defaults to 2.
#'
#' @return Invisibly returns a list indicating pipeline outcome:
#'   \code{success}: Logical (TRUE if insertion completed without error, FALSE otherwise).
#'   \code{message}: Character string describing the outcome or error.
#'   \code{symbols_processed}: Character vector of unique symbols in the data that
#'     was attempted to be inserted (useful even on failure). NULL if fetch failed.
#'
#' @export
#' @examples
#' \dontrun{
#' # --- Assumes you have defined ---
#' # log_summary(message_text)
#' # yahoo_query_data(symbols, date_from, date_to)
#' # format_data(data, digits)
#' # insert_new_data(db_connection, table_name, new_data_df)
#'
#' # --- Prerequisites ---
#' # install.packages("tidyquant")
#' # install.packages("RSQLite") # Example backend
#' # install.packages("dplyr") # Often needed by format_data
#' library(tidyquant)
#' library(DBI)
#' library(dplyr)
#'
#' # --- Setup Example Database ---
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#' try(dbExecute(con, "CREATE TABLE stock_data (
#'                  symbol TEXT, date TEXT, open REAL, high REAL,
#'                  low REAL, close REAL, volume REAL, adjusted REAL)"), silent = TRUE)
#'
#' # --- Define Symbols & Dates ---
#' symbols <- c("AAPL", "GOOG") # Example symbols
#' start_date <- Sys.Date() - 60 # Last 60 days
#'
#' # --- Run the Pipeline ---
#' pipeline_result <- start_pipeline(
#'   symbols_to_process = symbols,
#'   db_connection = con,
#'   table_name = "stock_data",
#'   date_from = start_date
#' )
#'
#' # --- Check Results ---
#' print(pipeline_result)
#' if (pipeline_result$success) {
#'   print("Data in DB:")
#'   print(dbReadTable(con, "stock_data"))
#' }
#'
#' # --- Clean up ---
#' dbDisconnect(con)
#' }
#'
start_pipeline <- function(symbols_to_process,
                           db_connection,
                           table_name,
                           date_from,
                           date_to = Sys.Date(),
                           format_digits = 2) {

  log_summary("Pipeline started.")
  outcome_message <- ""
  pipeline_success <- FALSE
  processed_symbols <- NULL

  # 1. Fetch Data (All symbols at once)
  log_summary(paste("Fetching data for symbols:", paste(symbols_to_process, collapse=", ")))
  raw_data <- yahoo_query_data(symbols = symbols_to_process,
                               date_from = date_from,
                               date_to = date_to)

  # Check if data fetch returned anything
  if (nrow(raw_data) == 0) {
    outcome_message <- "No data fetched or fetch failed. Stopping."
    log_summary(outcome_message)
    # Return failure information
    return(invisible(list(success = FALSE, message = outcome_message, symbols_processed = NULL)))
  }

  # Extract symbols actually returned by the query
  processed_symbols <- unique(raw_data$symbol)
  log_summary(paste(" -> Data fetched for:", paste(processed_symbols, collapse = ", ")))


  # 2. Format Data
  log_summary("Formatting fetched data...")
  formatted_data <- tryCatch({
    # Assuming format_data works on the whole tibble
    format_data(raw_data, digits = format_digits)
  }, error = function(e) {
    outcome_message <<- paste("Failed to format data:", e$message) # Assign to outer scope
    log_summary(paste(" -> Error during formatting:", outcome_message))
    return(NULL) # Indicate failure
  })

  if (is.null(formatted_data)) {
    # Return failure information
    return(invisible(list(success = FALSE, message = outcome_message, symbols_processed = processed_symbols)))
  }
  log_summary(" -> Formatting successful.")


  # 3. Insert Data
  log_summary(paste("Inserting data into table:", table_name))
  insert_success <- tryCatch({
    insert_new_data(db_connection = db_connection,
                    table_name = table_name,
                    new_data_df = formatted_data)
    TRUE # Return TRUE if dbAppendTable does not error
  }, error = function(e) {
    outcome_message <<- paste("Failed to insert data:", e$message) # Assign to outer scope
    log_summary(paste(" -> Error during insertion:", outcome_message))
    return(FALSE) # Indicate failure
  })


  # 4. Final Log and Return
  if (insert_success) {
    pipeline_success <- TRUE
    outcome_message <- "Pipeline finished successfully."
    log_summary(outcome_message)
  } else {
    log_summary("Pipeline finished with errors during insertion.")
    # outcome_message was set in the tryCatch error handler
  }

  invisible(list(
    success = pipeline_success,
    message = outcome_message,
    symbols_processed = processed_symbols
  ))
}
