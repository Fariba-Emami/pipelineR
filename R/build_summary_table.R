#' Summarize Column Value Counts
#'
#' @description Counts unique values in a specified column of a data frame.
#'
#' @param data A data frame.
#' @param summary_col The bare (unquoted) name of the column to summarize.
#'
#' @return A data frame (tibble) with counts of unique values, sorted
#'   descending by count. The columns will be named after `summary_col` and `n`.
#'   Stops with an error if the column doesn't exist.
#'
#' @importFrom dplyr count
#' @importFrom glue glue glue_data
#' @importFrom rlang !! ensym as_string
#' @export
#' @examples
#' df <- data.frame(
#'   category = sample(c("A", "B", "C"), 10, replace = TRUE),
#'   value = rnorm(10)
#' )
#'
#' summary_df <- build_summary_table(df, category)
#' print(summary_df)
#' #>   category n
#' #> 1        C 4
#' #> 2        B 3
#' #> 3        A 3
#'
build_summary_table <- function(data, summary_col) {

  col_sym <- rlang::ensym(summary_col)
  col_name <- rlang::as_string(col_sym)

  if (!col_name %in% names(data)) {
    stop(paste0("Column '", col_name, "' not found in data frame."), call. = FALSE)
  }

  summary_df <- dplyr::count(data, !!col_sym, sort = TRUE)

  return(summary_df)
}
