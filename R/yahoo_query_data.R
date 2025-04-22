#' Query Yahoo Finance Stock Price Data
#'
#' @description Fetches daily Open, High, Low, Close, Volume, and Adjusted
#'   stock price data from Yahoo Finance for given symbols and dates using
#'   `tidyquant::tq_get`. Includes basic logging.
#'
#' @param symbols A character vector of one or more stock ticker symbols.
#' @param date_from Character string or Date object. The start date for the data query
#'   (e.g., "YYYY-MM-DD").
#' @param date_to Character string or Date object. The end date for the data query.
#'   Defaults to the current system date (`Sys.Date()`).
#'
#' @return A tibble (data frame) containing the stock price data with columns
#'   like `symbol`, `date`, `open`, `high`, `low`, `close`, `volume`, `adjusted`.
#'   Returns an empty tibble if no valid symbols are provided, if data fetching fails,
#'   or if no data is found for the specified symbols/dates (with warnings).
#'   Stops with an error if `tidyquant` is not installed or `date_from` is missing.
#'
#' @importFrom tidyquant tq_get
#' @importFrom rlang abort warn is_character
#' @importFrom tibble tibble is_tibble
#' @export
#' @examples
#' \dontrun{ # Requires tidyquant installation and internet connection
#' # Assuming log_summary is defined: log_summary <- function(x) message(paste(Sys.time(), x))
#' library(tidyquant)
#'
#' # Get data for Apple for the start of 2023
#' aapl_data <- yahoo_query_data(symbols = "AAPL", date_from = "2023-01-01", date_to = "2023-01-10")
#' print(head(aapl_data))
#'
#' # Get data for multiple symbols for the last 30 days
#' multi_data <- yahoo_query_data(symbols = c("MSFT", "GOOG"), date_from = Sys.Date() - 30)
#' print(head(multi_data))
#'
#' # Example of a symbol/date range with potentially no data
#' future_data <- yahoo_query_data("AAPL", date_from = Sys.Date() + 1, date_to = Sys.Date() + 5)
#' print(future_data) # Should be empty tibble with warning
#' }
#'
yahoo_query_data <- function(symbols, date_from, date_to = Sys.Date()) {

  # --- Basic Input Validation ---
  if (!requireNamespace("tidyquant", quietly = TRUE)) {
    rlang::abort("Package 'tidyquant' is required. Please install it.")
  }
  # Ensure log_summary exists (or use message directly if not part of a package)
  if (!exists("log_summary", mode = "function")) {
    log_summary <- function(x) message(paste("[", Sys.time(), "]", x)) # Simple fallback logger
  }

  if (missing(symbols) || !rlang::is_character(symbols) || length(symbols) == 0) {
    log_summary("Warning: No valid 'symbols' provided to yahoo_query_data.")
    rlang::warn("No valid 'symbols' provided. Returning empty tibble.")
    return(tibble::tibble())
  }
  if (missing(date_from)) {
    rlang::abort("'date_from' must be provided for yahoo_query_data.")
  }
  # Note: tq_get is generally robust to date formats

  symbol_string <- paste(symbols, collapse=", ")
  log_summary(paste("Attempting to query Yahoo Finance for:", symbol_string))

  # --- Core Logic: Call tq_get ---
  data_result <- tryCatch({
    tidyquant::tq_get(x = symbols,
                      get = "stock.prices",
                      from = date_from,
                      to = date_to)
  }, error = function(e) {
    log_summary(paste("Error fetching data from Yahoo Finance for", symbol_string, ":", e$message))
    rlang::warn(paste("Failed to fetch data from Yahoo Finance:", e$message))
    return(NULL) # Signal an error occurred
  })

  # --- Handle Results ---
  if (is.null(data_result)) {
    # Error occurred during fetch, return empty tibble
    log_summary(paste(" -> Fetch failed for:", symbol_string))
    return(tibble::tibble())
  } else if (!tibble::is_tibble(data_result) || nrow(data_result) == 0) {
    # tq_get succeeded but returned no data (or something unexpected)
    log_summary(paste(" -> No data returned from Yahoo Finance for:", symbol_string, " (or result wasn't a tibble)."))
    rlang::warn(paste("No data found for symbols:", symbol_string, "in the specified date range."))
    return(tibble::tibble()) # Return empty tibble
  } else {
    # Success! Data was fetched.
    log_summary(paste(" -> Successfully fetched", nrow(data_result), "rows for:", symbol_string))
    return(data_result)
  }
}
