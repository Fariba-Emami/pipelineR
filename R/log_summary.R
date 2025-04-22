# --- log_summary() function (Append to Log Tibble) ---
#' Append Log Entry to Summary Tibble
#' @description Adds a new row representing a log event to a log summary tibble.
#' @param log_df The existing log summary tibble.
#' @param batch_id Character string identifier for the batch/step.
#' @param status Character string indicating outcome (e.g., "OK", "ERROR").
#' @param message_text Character string providing details.
#' @return The `log_df` tibble with the new log entry appended.
#' @importFrom tibble add_row tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang abort warn is_character
#' @export
log_summary <- function(log_df, batch_id = "N/A", status = "INFO", message_text = "") {
  if (!requireNamespace("tibble", quietly = TRUE)) { rlang::abort("Package 'tibble' required.") }
  if (!is.data.frame(log_df)) { rlang::abort("'log_df' must be a df/tibble.") }
  expected_cols <- c("timestamp", "batch_id", "status", "message")
  if(!all(expected_cols %in% names(log_df))) { rlang::abort("Input 'log_df' missing expected columns.") }
  if(!rlang::is_character(batch_id)||!rlang::is_character(status)||!rlang::is_character(message_text)){
    rlang::warn("Inputs not character, attempting coercion.") }

  new_log_entry <- tibble::tibble(
    timestamp = Sys.time(), batch_id = as.character(batch_id),
    status = toupper(as.character(status)), message = as.character(message_text) )
  # Use bind_rows which handles 0-row log_df correctly
  log_df <- dplyr::bind_rows(log_df, new_log_entry)
  return(log_df)
}
