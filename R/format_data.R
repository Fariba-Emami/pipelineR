# --- format_data() function (No changes from previous version) ---
#' Format Stock Data to Long Format
#' @description Reshapes wide stock data (from `yahoo_query_data`) to long format.
#' @param wide_data Data frame/tibble with columns 'symbol', 'date', 'open', etc.
#' @return Tibble with columns 'symbol', 'date', 'metric', 'value'. Empty tibble on error/bad input.
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select any_of
#' @importFrom rlang abort warn
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @export
format_data <- function(wide_data) {
  if (!requireNamespace("tidyr", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    rlang::abort("Packages 'tidyr' and 'dplyr' required.") }
  if(!is.data.frame(wide_data) || nrow(wide_data) == 0) {
    rlang::warn("Input 'wide_data' empty/not df. Returning empty tibble."); return(tibble::tibble()) }

  required_cols <- c("symbol", "date"); measure_cols <- c("open", "high", "low", "close", "volume", "adjusted")
  if (!all(required_cols %in% names(wide_data))) {
    rlang::warn("Input missing 'symbol'/'date'. Returning empty tibble."); return(tibble::tibble()) }
  cols_to_pivot <- intersect(measure_cols, names(wide_data))
  if(length(cols_to_pivot) == 0) {
    rlang::warn("Input missing measure columns (open, etc.). Returning empty tibble."); return(tibble::tibble()) }

  long_data <- tryCatch({ wide_data %>%
      tidyr::pivot_longer(cols = dplyr::any_of(cols_to_pivot), names_to = "metric", values_to = "value") %>%
      dplyr::select(dplyr::any_of(c("symbol", "date", "metric", "value")))
  }, error = function(e){ rlang::abort(paste("pivot_longer failed:", e$message)) })
  return(long_data)
}
