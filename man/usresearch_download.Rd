% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ffdata_download.R
\name{usresearch_download}
\alias{usresearch_download}
\title{The usresearch_download function}
\usage{
usresearch_download(
  base,
  dir,
  sub_path,
  number_factors,
  freq,
  start,
  end,
  clean_na
)
}
\arguments{
\item{base}{a character string, the Kenneth R. French Data Library address.}

\item{dir}{a character string, the directory for saving the data.
The current working directory is set by default.}

\item{sub_path}{a character string, the folder subpath, created by ffDataDownload.}

\item{number_factors}{an integer, the number of factors for the USResearch portfolios.
Possible values are 3 (default) and 5.}

\item{freq}{a character string, the frequency of returns.
Possible values are "m" for monthly (default) or "d" for daily.}

\item{start}{a character string, start date for the download in the format "YYmm".
Default value is 197501.}

\item{end}{a character string, end date for the download in the format "YYmm".
Default value is two months before Sys.Date() to insure availability.}

\item{clean_na}{a logical, TRUE (default) replaces NAs with zoo::na.locf(). If FALSE, NAs are not cleaned.}
}
\value{
a .csv-file within the directory, defined in dir.
}
\description{
This function automatically downloads US-Research portfolios returns
from the Kenneth R. French Data Library.
The downloaded returns are saved then as a .csv-file in the chosen directory.
}
