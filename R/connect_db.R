#' Connect to PostgreSQL Database
#'
#' @description Establishes a PostgreSQL connection using environment variables
#'   (`PG_DB`, `PG_HOST`, `PG_USER`, `PG_PASSWORD`). Port is fixed at 5432.
#'
#' @return A `DBIConnection` object. Remember to disconnect with `DBI::dbDisconnect()`.
#' @author fariba
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RPostgres Postgres
#' @export
#' @examples
#' \dontrun{
#' # Ensure PG_* environment variables are set first
#' con <- connect_db()
#' DBI::dbDisconnect(con)
#' }

connect_db <- function() {
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("PG_DB"),
    host = Sys.getenv("PG_HOST"),
    user = Sys.getenv("PG_USER"),
    password = Sys.getenv("PG_PASSWORD"),
    port = 5432
  )
  return(con)
}
