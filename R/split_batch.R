# --- split_batch() function (No changes from previous version) ---
#' Split Items into Batches
#' @description Divides a vector into a list of smaller vectors (batches).
#' @param items A vector containing items to split.
#' @param batch_size Integer. Max number of items per batch.
#' @return A list, where each element is a batch. Returns empty list if items is empty.
#' @importFrom rlang abort
#' @export
split_batch <- function(items, batch_size) {
  n <- length(items)
  if (!is.numeric(batch_size) || length(batch_size) != 1 || batch_size <= 0 || batch_size %% 1 != 0) {
    rlang::abort("'batch_size' must be a single positive integer.")
  }
  if (n == 0) { return(list()) }
  grouping_indices <- ((seq_len(n) - 1) %/% batch_size) + 1
  batches <- split(items, f = grouping_indices)
  names(batches) <- NULL
  return(batches)
}
