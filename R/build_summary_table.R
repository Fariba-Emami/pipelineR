#' Initialize an empty summary table
#'
#' This function creates an empty tibble to collect the processing summary.
#'
#' @return A tibble with the columns: batch_id, symbol, status, n_rows, message, timestamp.
#' @export
build_summary_table <- function() {
  tibble::tibble(
    batch_id = integer(),
    symbol = character(),
    status = character(),
    n_rows = integer(),
    message = character(),
    timestamp = lubridate::now()
  )
}
