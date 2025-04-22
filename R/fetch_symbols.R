#' Fetch Stock Symbols from a Specified Index
#'
#' @description Uses the `tidyquant` package to retrieve the constituent
#'   symbols for a given stock market index (e.g., S&P 500, NASDAQ).
#'
#' @param index A character string specifying the index to fetch symbols for.
#'   Common values include "SP500", "NASDAQ", "DOW". See `tidyquant::tq_index_options()`
#'   for available options. Defaults to "SP500".
#' @param silent Logical. If `TRUE`, suppresses messages from `tq_index`. Defaults to `FALSE`.
#'
#' @return A character vector of stock symbols belonging to the specified index.
#'   Returns `NULL` and prints an error if the index is not found or the
#'   `tidyquant` package is not installed.
#'
#' @importFrom dplyr pull
#' @importFrom tidyquant tq_index tq_index_options
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' # Ensure tidyquant is installed: install.packages("tidyquant")
#'
#' sp500_symbols <- fetch_symbols(index = "SP500")
#' head(sp500_symbols)
#'
#' nasdaq_symbols <- fetch_symbols(index = "NASDAQ")
#' head(nasdaq_symbols)
#' }
fetch_symbols <- function(index = "SP500", silent = FALSE) {

  # Check if tidyquant is installed (moved from yahoo_query_data example, good here too)
  if (!requireNamespace("tidyquant", quietly = TRUE)) {
    stop("Package 'tidyquant' is required for this function. Please install it.", call. = FALSE)
  }

  # Validate index choice (optional but good practice from previous example)
  available_indices <- tidyquant::tq_index_options()
  if (!index %in% available_indices) {
    warning("Index '", index, "' not found in tidyquant::tq_index_options(). ",
            "Attempting to fetch anyway, but it might fail.", call. = FALSE)
  }


  symbols_data <- tryCatch({
    tidyquant::tq_index(index, use_fallback = TRUE)
  }, error = function(e) {
    message("Error fetching index '", index, "': ", e$message)
    return(NULL) # Return NULL on error
  })

  # Check if data retrieval was successful
  if (is.null(symbols_data) || nrow(symbols_data) == 0) {
    message("Could not retrieve symbols for index: ", index)
    return(character(0)) # Return empty character vector
  }

  # Extract the 'symbol' column using the .data pronoun
  symbols_vector <- dplyr::pull(symbols_data, .data$symbol) # <-- CORRECTED HERE

  if (!silent) {
    message("Successfully fetched ", length(symbols_vector), " symbols for index '", index, "'.")
  }

  return(symbols_vector)
}
