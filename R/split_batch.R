#' Split a tibble of symbols into batches
#'
#' This function splits a tibble into a list of smaller tibbles, each containing up to `batch_size` rows.
#'
#' Batching is important because querying too many symbols at once from external APIs (like Yahoo Finance)
#' can lead to:
#' - API rate limits being exceeded,
#' - Timeout errors,
#' - Partial or corrupted data responses.
#'
#' By splitting requests into manageable batches, we ensure more stable API calls, reduce failure rates,
#' and improve overall pipeline reliability.
#'
#' @param symbol_list A tibble containing at least a column `symbol`. Typically output from `fetch_symbols()`.
#' @param batch_size An integer indicating the maximum number of rows per batch. Default is 25.
#'
#' @return A list of tibbles, each containing up to `batch_size` symbols.
#' @export
split_batch <- function(symbol_list, batch_size = 25) {

  if (!tibble::is_tibble(symbol_list)) {
    stop("'symbol_list' must be a tibble.")
  }

  n <- nrow(symbol_list)

  if (n == 0) {
    warning("Input 'symbol_list' is empty.")
    return(list())
  }

  # Create sequence of indices for batching
  batch_indices <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

  # Split the tibble into batches
  batches <- lapply(batch_indices, function(idx) symbol_list[idx, , drop = FALSE])

  return(batches)
}
