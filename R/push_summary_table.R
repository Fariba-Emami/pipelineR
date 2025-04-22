#' Push (Save) Summary Table to CSV (Minimal)
#'
#' @description Writes a data frame to a specified CSV file using `write.csv`.
#'
#' @param summary_df A data frame.
#' @param file_path Character string. The output CSV file path.
#'
#' @return No return value (`NULL`). Called for its side effect of writing a file.
#'   Will stop with an error from `write.csv` if inputs are wrong or writing fails.
#' @export
#' @examples
#' # Sample data
#' summary_data <- data.frame(Item = c("X", "Y"), Count = c(10, 20))
#' temp_path <- tempfile(fileext = ".csv")
#'
#' # Save the data
#' push_summary_table(summary_data, temp_path)
#'
#' # Verify (optional)
#' if(file.exists(temp_path)) {
#'   print(paste("File written to:", temp_path))
#'   # unlink(temp_path) # Clean up
#' }
#'
push_summary_table <- function(summary_df, file_path) {

  utils::write.csv(x = summary_df, file = file_path, row.names = FALSE)
}
