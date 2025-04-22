#' Split Items into Batches
#'
#' @description Divides a vector or list of items into smaller batches (lists)
#'   of a specified maximum size using base R `split`.
#'
#' @param items A vector or list containing the items to be split.
#' @param batch_size An integer specifying the maximum number of items in each batch.
#'   Must be positive.
#'
#' @return A list, where each element is a batch (a vector or list containing
#'   a subset of the original `items`). The last batch may contain fewer items
#'   than `batch_size`. Returns an empty list if `items` is empty.
#'
#' @export
#' @examples
#' symbols <- paste0("SYM", 1:23) # 23 items
#'
#' # Split into batches of 5
#' batches_5 <- split_batch(symbols, 5)
#' print(length(batches_5)) # Should be 5 batches
#' print(batches_5[[1]])    # First batch
#' print(batches_5[[5]])    # Last batch (smaller)
#'
#' # Split into batches of 10
#' batches_10 <- split_batch(symbols, 10)
#' print(length(batches_10)) # Should be 3 batches
#'
#' # Edge case: Empty input
#' empty_batches <- split_batch(character(0), 5)
#' print(empty_batches) # Should be list()
#'
split_batch <- function(items, batch_size) {

  n <- length(items)

  if (!is.numeric(batch_size) || length(batch_size) != 1 || batch_size <= 0 || batch_size %% 1 != 0) {
    stop("'batch_size' must be a single positive integer.", call. = FALSE)
  }

  if (n == 0) {
    return(list())
  }


  grouping_indices <- ((seq_len(n) - 1) %/% batch_size) + 1

  batches <- split(items, f = grouping_indices)

  return(batches)
}
