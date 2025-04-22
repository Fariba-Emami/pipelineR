#' Format Stock OHLCV and Adjusted Data (Rounding)
#'
#' @description Rounds standard price columns (open, high, low, close, adjusted)
#'   and includes the volume column in the formatting process (though rounding
#'   typically doesn't affect integer volume). Works on data frames obtained
#'   from `tidyquant`.
#'
#' @param data A data frame, typically the output from
#'   `tidyquant::tq_get(get = "stock.prices")`.
#' @param digits Integer. The number of decimal places to round the numeric columns to.
#'   Defaults to 2.
#'
#' @return The data frame with the relevant numeric OHLC, Volume, and Adjusted columns
#'   processed (typically rounded).
#'   It only attempts to format columns that exist and are numeric.
#'
#' @importFrom dplyr mutate across any_of select where
#' @importFrom rlang abort
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{ # Requires tidyquant installation
#' library(tidyquant)
#'
#' # Get some sample data
#' stock_data_raw <- tq_get("AAPL", get = "stock.prices", from = "2023-01-01", to = "2023-01-10")
#' print(head(stock_data_raw, 3)) # Original data
#'
#' # Format the data (round prices, process volume)
#' stock_data_formatted <- format_data(stock_data_raw, digits = 2)
#' print(head(stock_data_formatted, 3)) # Formatted data (volume likely unchanged by round)
#' }
#'
format_data <- function(data, digits = 2) {


  if (!is.data.frame(data)) {
    rlang::abort("Input 'data' must be a data frame.")
  }
  if (!is.numeric(digits) || length(digits) != 1 || digits < 0 || digits %% 1 != 0) {
    rlang::abort("'digits' must be a single non-negative integer.")
  }

  target_cols <- c("open", "high", "low", "close", "volume", "adjusted")


  cols_present <- intersect(target_cols, names(data))

  if (length(cols_present) == 0) {
    warning("No standard columns (open, high, low, close, volume, adjusted) found. Returning original data.", call. = FALSE)
    return(data)
  }


  numeric_cols_to_format <- data %>%
    dplyr::select(dplyr::any_of(cols_present)) %>%
    dplyr::select(dplyr::where(is.numeric)) %>%
    names()

  if (length(numeric_cols_to_format) == 0) {
    warning("None of the standard target columns found were numeric. Returning original data.", call. = FALSE)
    return(data)
  }


  data <- dplyr::mutate(data,
                        dplyr::across(dplyr::all_of(numeric_cols_to_format), ~ round(.x, digits = digits))
  )

  return(data)
}
