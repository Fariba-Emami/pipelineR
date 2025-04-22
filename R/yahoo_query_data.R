# --- yahoo_query_data() function (No changes from previous version) ---
#' Query Yahoo Finance Stock Price Data
#' @description Fetches daily OHLCV + Adjusted data from Yahoo Finance using `tidyquant`.
#' @param symbols Character vector of stock ticker symbols.
#' @param date_from Start date (character "YYYY-MM-DD" or Date).
#' @param date_to End date (character "YYYY-MM-DD" or Date). Defaults to `Sys.Date()`.
#' @return A tibble with stock price data. Returns empty tibble on error/no data (with warnings).
#' @importFrom tidyquant tq_get
#' @importFrom rlang abort warn is_character
#' @importFrom tibble tibble is_tibble
#' @export
yahoo_query_data <- function(symbols, date_from, date_to = Sys.Date()) {
  if (!requireNamespace("tidyquant", quietly = TRUE)) { rlang::abort("Package 'tidyquant' required.") }
  if (missing(symbols) || !rlang::is_character(symbols) || length(symbols) == 0) {
    rlang::warn("No valid 'symbols' provided. Returning empty tibble."); return(tibble::tibble()) }
  if (missing(date_from)) { rlang::abort("'date_from' must be provided.") }

  symbol_string <- paste(symbols, collapse=", ")
  # Consider adding a log_summary() call here if you have it defined globally

  data_result <- tryCatch({ tidyquant::tq_get(x = symbols, get = "stock.prices", from = date_from, to = date_to)
  }, error = function(e) { rlang::warn(paste("Yahoo fetch failed for", symbol_string, ":", e$message)); return(NULL) })

  if (is.null(data_result)) { return(tibble::tibble())
  } else if (!tibble::is_tibble(data_result) || nrow(data_result) == 0) {
    rlang::warn(paste("No Yahoo data returned for", symbol_string)); return(tibble::tibble())
  } else { return(data_result) }
}
