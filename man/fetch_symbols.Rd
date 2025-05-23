% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_symbols.R
\name{fetch_symbols}
\alias{fetch_symbols}
\title{Fetch S&P 500 symbols from the database}
\usage{
fetch_symbols(con)
}
\arguments{
\item{con}{A valid DBI database connection.}
}
\value{
A tibble containing two columns: \code{symbol} and \code{index_ts}.
If no symbols are found, a warning is issued and an empty tibble is returned.
}
\description{
This function retrieves a list of distinct symbols and their corresponding index_ts
from the \code{sp500.info} table in the PostgreSQL database.
}
