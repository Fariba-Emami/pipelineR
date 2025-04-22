# --- insert_new_data() function (anti_join version) ---
#' Insert New Data into Database Table, Avoiding Duplicates
#' @description Appends rows to a database table, checking 'symbol', 'date',
#'   'metric' keys to avoid duplicates. Assumes long format input.
#' @param db_connection A valid DBIConnection object.
#' @param table_name Character string: Target database table (e.g., "data_sp500").
#' @param new_data_df Data frame/tibble in long format (symbol, date, metric, value).
#' @return Invisibly returns number of rows inserted. Warns/stops on error. Returns 0 on empty input/all duplicates.
#' @importFrom DBI dbIsValid dbReadTable dbAppendTable dbQuoteIdentifier
#' @importFrom dplyr anti_join select distinct across any_of filter bind_rows
#' @importFrom rlang abort warn inform .data
#' @importFrom magrittr %>%
#' @importFrom tibble tibble as_tibble
#' @export
insert_new_data <- function(db_connection, table_name, new_data_df) {
  if (!DBI::dbIsValid(db_connection)) { rlang::abort("Invalid database connection.") }
  if (!is.data.frame(new_data_df)) { rlang::warn("Input 'new_data_df' not df."); return(invisible(0)) }
  if (nrow(new_data_df) == 0) { rlang::inform("Input 'new_data_df' empty."); return(invisible(0)) }
  if (!requireNamespace("dplyr", quietly = TRUE)) { rlang::abort("Package 'dplyr' required.") }

  required_cols <- c("symbol", "date", "metric", "value")
  if (!all(required_cols %in% names(new_data_df))) { rlang::abort("Input df missing columns.") }

  key_cols <- c("symbol", "date", "metric")
  new_keys_df <- tryCatch(tibble::as_tibble(new_data_df) %>% dplyr::distinct(dplyr::across(dplyr::any_of(key_cols))), error = function(e) {
    rlang::warn(paste("Error creating distinct keys from new data:", e$message)); return(tibble::tibble())})
  if (nrow(new_keys_df) == 0) { rlang::inform("No distinct keys in new data."); return(invisible(0)) }

  # Fetch existing keys - Simplified: reads whole table keys. Optimize if table is huge.
  existing_data <- tryCatch({
    key_cols_sql <- paste(DBI::dbQuoteIdentifier(db_connection, key_cols), collapse = ", ")
    table_name_sql <- DBI::dbQuoteIdentifier(db_connection, table_name)
    sql <- paste("SELECT DISTINCT", key_cols_sql, "FROM", table_name_sql)
    tibble::as_tibble(DBI::dbGetQuery(db_connection, sql))
  }, error = function(e) {
    if (grepl("does not exist|relation .* does not exist", e$message, ignore.case = TRUE)) {
      rlang::inform(paste("Table", table_name, "not found. Assuming no existing keys."))
      return(tibble::tibble()) } else { rlang::abort(paste("Failed read keys from", table_name, ":", e$message)) } })

  # Find rows to insert
  if (nrow(existing_data) > 0) {
    # Coerce key types if needed before join (dates can be tricky)
    existing_data <- existing_data %>% dplyr::mutate(dplyr::across(dplyr::any_of("date"), as.Date))
    new_data_df_mod <- new_data_df %>% dplyr::mutate(dplyr::across(dplyr::any_of("date"), as.Date))
    rows_to_insert <- dplyr::anti_join(new_data_df_mod, existing_data, by = key_cols)
  } else { rows_to_insert <- new_data_df }

  n_to_insert <- nrow(rows_to_insert)
  if (n_to_insert > 0) {
    rlang::inform(paste("Attempting to insert", n_to_insert, "new rows into", table_name))
    success <- tryCatch({ DBI::dbAppendTable(db_connection, table_name, rows_to_insert); TRUE
    }, error = function(e) { rlang::warn(paste("Append failed to", table_name, ":", e$message)); return(FALSE) })
    if (!success) n_to_insert <- 0
  } else { rlang::inform("No new rows to insert.") }
  invisible(n_to_insert)
}
