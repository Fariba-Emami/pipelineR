# --- start_pipeline() function (Updated Orchestrator) ---
#' Run the Full Stock Data ETL Pipeline (Database Focused)
#' @description Orchestrates the process: connects to DB, fetches symbols from DB,
#'   processes in batches to get Yahoo data, formats, inserts new records into data table,
#'   logs results, and pushes logs to DB.
#' @param batch_size Integer. Max symbols per API call batch. Defaults to 50.
#' @param symbol_table_name Character string. DB table containing symbols. Defaults to "sp500".
#' @param symbol_col_name Character string. Column name for symbols in symbol table. Defaults to "symbol".
#' @param data_table_name Character string. Main DB table for stock data. Defaults to "data_sp500".
#' @param log_table_name Character string. DB table for logs. Defaults to "pipeline_logs".
#' @param date_from Character string or Date. Start date for fetching data. Required.
#' @param date_to Character string or Date. End date for fetching data. Defaults to `Sys.Date()`.
#' @param db_connection Optional. An existing DBI connection. If NULL (default),
#'   `connect_db()` is called internally and disconnected upon exit.
#' @return Invisibly returns the final log data frame.
#' @importFrom rlang inform abort warn
#' @importFrom DBI dbDisconnect dbIsValid
#' @export
#' @examples
#' \dontrun{
#' # Ensure env vars for connect_db() are set.
#' # Ensure DB tables (sp500, data_sp500, pipeline_logs) exist.
#' final_logs <- start_pipeline(date_from = Sys.Date() - 7)
#' print(final_logs)
#' }
start_pipeline <- function(batch_size = 50,
                           symbol_table_name = "sp500",
                           symbol_col_name = "symbol",
                           data_table_name = "data_sp500",
                           log_table_name = "pipeline_logs",
                           date_from,
                           date_to = Sys.Date(),
                           db_connection = NULL) {

  # --- Setup ---
  log_df <- build_summary_table() # Initialize empty log df
  con <- NULL
  created_connection <- FALSE
  final_log_push_status <- NULL # Use NULL to track if push happens

  log_df <- log_summary(log_df, "Setup", "INFO", paste("Pipeline started at", Sys.time()))

  # --- Cleanup Handler ---
  on.exit({
    if (created_connection && !is.null(con) && DBI::dbIsValid(con)) {
      message("\nDisconnecting database connection.")
      DBI::dbDisconnect(con)
    }
    # Check final log status carefully
    if(!is.null(final_log_push_status)){
      if(!final_log_push_status) {
        warning("Final log push to database failed. See returned log data frame.", call. = FALSE)
      }
    } else {
      warning("Pipeline exited before final log push attempt. Check returned log data frame.", call. = FALSE)
    }
    message("Pipeline execution finished.")
  })

  # --- Database Connection ---
  log_df <- log_summary(log_df, "Setup", "INFO", "Handling database connection...")
  if (is.null(db_connection)) {
    con <- tryCatch(connect_db(), error = function(e) {
      log_df <<- log_summary(log_df, "Setup", "FATAL", paste("Connect DB failed:", e$message)) # Log failure
      NULL })
    if (is.null(con)) rlang::abort("Database connection failed. Cannot proceed.")
    created_connection <- TRUE
  } else if (DBI::dbIsValid(db_connection)) {
    con <- db_connection
    log_df <- log_summary(log_df, "Setup", "INFO", "Using provided DB connection.")
  } else {
    log_df <- log_summary(log_df, "Setup", "FATAL", "Provided DB connection is not valid.")
    rlang::abort("Provided database connection is not valid.")
  }

  # --- Fetch Symbols (from DB) ---
  log_df <- log_summary(log_df, "Fetch", "INFO", paste("Fetching symbols from DB table:", symbol_table_name))
  # Pass the established connection 'con'
  symbols <- fetch_symbols(db_connection = con,
                           table_name = symbol_table_name,
                           symbol_col_name = symbol_col_name)
  if (length(symbols) == 0) {
    log_df <- log_summary(log_df, "Fetch", "ERROR", "Failed to fetch symbols from DB or list is empty. Stopping.")
    final_log_push_status <- push_summary_table(con, log_df, log_table_name) # Try to log failure
    return(invisible(log_df))
  }
  log_df <- log_summary(log_df, "Fetch", "OK", paste("Fetched", length(symbols), "symbols from DB."))

  # --- Split into Batches ---
  batches <- split_batch(symbols, batch_size)
  n_batches <- length(batches)
  log_df <- log_summary(log_df, "Setup", "INFO", paste("Split symbols into", n_batches, "batches."))

  # --- Process Batches ---
  total_rows_inserted <- 0
  for (i in seq_along(batches)) {
    current_batch_symbols <- batches[[i]]
    batch_label <- paste0("Batch ", i, "/", n_batches)
    symbol_string <- paste(current_batch_symbols, collapse=", ")
    log_df <- log_summary(log_df, batch_label, "INFO", paste("Processing symbols:", symbol_string))

    # Query Yahoo Data
    raw_data <- yahoo_query_data(current_batch_symbols, date_from, date_to) # Handles its errors/warnings
    if (nrow(raw_data) == 0) {
      log_df <- log_summary(log_df, batch_label, "WARN", "No data from Yahoo Finance.")
      next
    }

    # Format Data
    long_data <- format_data(raw_data) # Handles its errors/warnings
    if (nrow(long_data) == 0) {
      log_df <- log_summary(log_df, batch_label, "WARN", "Formatting yielded no data.")
      next
    }

    # Insert Data
    rows_inserted_batch <- insert_new_data(con, data_table_name, long_data) # Handles errors/warnings
    if (is.numeric(rows_inserted_batch) && rows_inserted_batch >= 0) {
      log_df <- log_summary(log_df, batch_label, "OK", paste("Inserted", rows_inserted_batch, "new data rows."))
      total_rows_inserted <- total_rows_inserted + rows_inserted_batch
    } else {
      log_df <- log_summary(log_df, batch_label, "ERROR", "Data insertion step failed or returned unexpected value.")
    }
    # Optional: Add a small sleep to be nice to Yahoo's API
    # Sys.sleep(0.5)
  } # End batch loop

  # --- Final Log Push ---
  log_df <- log_summary(log_df, "Cleanup", "INFO",
                        paste("Pipeline processing complete. Total new rows inserted:", total_rows_inserted))
  log_df <- log_summary(log_df, "Cleanup", "INFO", "Pushing final logs to database...")
  final_log_push_status <- push_summary_table(con, log_df, log_table_name)

  # Return the full log data frame
  invisible(log_df)
}
