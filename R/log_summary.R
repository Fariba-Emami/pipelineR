#' Log a Simple Summary Message
#'
#' @description Prints a timestamped message to the console, typically used
#'   to log the completion or summary of an operation.
#'
#' @param message_text Character string. The message to log.
#'
#' @return Invisibly returns `NULL`. Its primary effect is printing to the console.
#' @export
#' @examples
#' log_summary("Data processing complete for AAPL.")
#' rows_inserted <- 50
#' log_summary(paste("Successfully inserted", rows_inserted, "records."))
#'
log_summary <- function(message_text) {

  # Get the current time
  timestamp <- Sys.time()

  # Format the log entry
  log_entry <- paste0("[", format(timestamp, "%Y-%m-%d %H:%M:%S"), "] ", message_text)

  # Print the message to the console
  message(log_entry)

  # Return NULL invisibly as the main point is the side effect (printing)
  invisible(NULL)
}
