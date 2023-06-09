% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get.data}
\alias{get.data}
\title{Download nowcasting data}
\usage{
get.data(
  m.file = NULL,
  q.file = NULL,
  y.name = "GDPC1",
  date_start = NULL,
  date_end = NULL,
  transform = TRUE,
  na.rm = TRUE
)
}
\arguments{
\item{m.file}{Argument to \code{fredmd}. Either a path to a file, a connection, or literal data (either a single string or a raw vector)}

\item{q.file}{Argument to \code{fredqd}.}

\item{y.name}{response variable}

\item{date_start}{Date or \code{NULL}, the start date (included) of the data selection.
If \code{NULL}, select till the latest data available.}

\item{date_end}{Date or \code{NULL}, the end date (included) of the data selection.
If \code{NULL}, select up to the earliest data available.}

\item{transform}{logical, indicating Whether or not the FRED-MD data set
should be transformed according to the transformation code.}

\item{na.rm}{remove rows and colums containing \code{NA}}
}
\value{
List containing \code{ts} objects:
\itemize{
  \item{\code{fmd}}{ a subset of the (transformed) FRED-MD data of class \code{fredmd}.}
  \item{\code{fqd}}{ a subset of the (transformed) FRED-QD data of class \code{fredmd}.}
  \item{\code{y}}{ response variable}
}
.
}
\description{
Downloads monthly and quarterly data from the FRED site
}
\examples{
nowcasting_data <- get.data()
}
