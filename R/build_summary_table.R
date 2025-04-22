# --- build_summary_table() function (Empty Log Structure) ---
#' Create Empty Log Tibble Structure
#' @description Defines and returns an empty tibble with predefined columns for logging.
#' @return An empty tibble with columns: timestamp, batch_id, status, message.
#' @importFrom tibble tibble
#' @export
build_summary_table <- function() {
  log_structure <- tibble::tibble(
    timestamp = Sys.time()[0], # Empty POSIXct
    batch_id = character(),    # Empty character
    status = character(),      # Empty character
    message = character()      # Empty character
  )
  return(log_structure)
}
