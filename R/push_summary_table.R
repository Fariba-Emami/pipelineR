#' Push Data to Database Table
#'
#' @description Appends data frame entries to a specified database table.
#'
#' @param db_connection A valid DBIConnection object.
#' @param data_df Data frame containing the data to append.
#' @param table_name Character string. Name of the target table. Defaults to "pipeline_logs".
#'
#' @return Invisibly returns TRUE if append was successful, FALSE otherwise.
#'
#' @importFrom DBI dbIsValid dbAppendTable
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(...)
#' logs <- data.frame(timestamp = Sys.time(), message = "Process completed")
#' push_summary_table(con, logs)
#' }
push_summary_table <- function(db_connection, data_df, table_name = "pipeline_logs") {
  # Check connection
  if (!DBI::dbIsValid(db_connection)) {
    stop("Invalid database connection.")
  }

  # Validate input data
  if (!is.data.frame(data_df)) {
    warning("Input must be a data frame.")
    return(invisible(FALSE))
  }

  # Handle empty data frame
  if (nrow(data_df) == 0) {
    message("Input data frame is empty.")
    return(invisible(TRUE))
  }

  # Attempt to append data
  message(paste("Pushing", nrow(data_df), "rows to", table_name))

  success <- tryCatch({
    DBI::dbAppendTable(db_connection, table_name, data_df)
    TRUE
  }, error = function(e) {
    warning(paste("Failed to append data to", table_name, ":", e$message))
    FALSE
  })

  invisible(success)
}
