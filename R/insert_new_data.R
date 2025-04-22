#' Insert Data into a Database Table (Simple)
#'
#' @description Appends a data frame to a specified database table using a
#'   DBI connection via `DBI::dbAppendTable`.
#'
#' @param db_connection A valid DBIConnection object (e.g., from `DBI::dbConnect`).
#' @param table_name A character string: the name of the target database table.
#' @param new_data_df A data frame with data to insert. Column names should
#'   match the target table's columns.
#'
#' @return Invisibly returns `TRUE` on success (from `DBI::dbAppendTable`).
#'   Throws an error on failure (e.g., table not found, column mismatch,
#'   connection issue).
#'
#' @importFrom DBI dbAppendTable
#' @export
#' @examples
#' \dontrun{
#' # --- Prerequisites ---
#' # install.packages("RSQLite") # Example backend
#' library(DBI)
#'
#' # --- Setup Example Database ---
#' # Create an in-memory SQLite database for demonstration
#' con <- dbConnect(RSQLite::SQLite(), ":memory:")
#'
#' # Create an initial table
#' initial_df <- data.frame(symbol = "AAPL", date = Sys.Date(), close = 150.0)
#' dbWriteTable(con, "stock_prices", initial_df)
#' print("Initial table:")
#' print(dbReadTable(con, "stock_prices"))
#'
#' # --- Data to Insert ---
#' df_to_insert <- data.frame(symbol = "MSFT", date = Sys.Date(), close = 280.5)
#'
#' # --- Use the function ---
#' success <- insert_new_data(
#'              db_connection = con,
#'              table_name = "stock_prices",
#'              new_data_df = df_to_insert
#'            )
#'
#' # --- Verify ---
#' if (success) {
#'   print("Data inserted successfully. Updated table:")
#'   print(dbReadTable(con, "stock_prices"))
#' }
#'
#' # --- Clean up ---
#' dbDisconnect(con)
#' }
#'
insert_new_data <- function(db_connection, table_name, new_data_df) {

  DBI::dbAppendTable(conn = db_connection, name = table_name, value = new_data_df)

}
