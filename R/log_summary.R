#' Log Summary
#'
#' Appends a new log entry to the summary table.
#'
#' @param summary_table The log tibble to append to.
#' @param batch_id Identifier of the batch.
#' @param symbol Stock ticker.
#' @param status "OK" or "ERROR".
#' @param n_rows Number of rows processed for this symbol.
#' @param message Any message (e.g., error text).
#'
#' @return Updated summary table with the new log entry.
#' @export
log_summary <- function(summary_table, batch_id, symbol, status, n_rows, message) {
  new_row <- tibble::tibble(
    batch_id = batch_id,
    symbol = symbol,
    status = status,
    n_rows = n_rows,
    message = message,
    timestamp = Sys.time()
  )

  dplyr::bind_rows(summary_table, new_row)
}
