## get fredmd data, from "fbi"


#' @title Loading FRED-MD Data Set
#'
#' @description \code{fredmd} loads the official FRED-MD data set and provides
#' a few tools to manipulate the data set.
#'
#' @import readr
#' @export
#'
#' @param file Either a path to a file, a connection, or literal data (either a single string or a raw vector).
#' @param date_start Date or \code{NULL}, the start date (included) of the data selection.
#' If \code{NULL}, select till the latest data available.
#' @param date_end Date or \code{NULL}, the end date (included) of the data selection.
#' If \code{NULL}, select up to the earliest data available.
#' @param transform logical, indicating Whether or not the FRED-MD data set
#' should be transformed according to the transformation code.
#' @return a subset of the (transformed) FRED-MD data of class \code{fredmd}.
#'
#' @author Yankang (Bennie) Chen <yankang.chen@@yale.edu>
#'
#' @references
#' Michael W. McCracken and Serena Ng (2015), \emph{FRED-MD and FRED-QD: Monthly and Quarterly Databases for Macroeconomic Research}.
#' \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
#' @examples fred_data <- fredmd()
fredmd <- function(file = NULL, date_start = NULL, date_end = NULL, transform = TRUE) {
  if(is.null(file)) file <- "https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv"
  # Error checking
  if (!is.logical(transform))
    stop("'transform' must be logical.")
  if ((class(date_start) != "Date") && (!is.null(date_start)))
    stop("'date_start' must be Date or NULL.")
  if ((class(date_end) != "Date") && (!is.null(date_end)))
    stop("'date_end' must be Date or NULL.")

  if (class(date_start) == "Date") {
    if (as.numeric(format(date_start, "%d")) != 1)
      stop("'date_start' must be Date whose day is 1.")
    if (date_start < as.Date("1959-01-01"))
      stop("'date_start' must be later than 1959-01-01.")
  }

  if (class(date_end) == "Date") {
    if (as.numeric(format(date_end, "%d")) != 1)
      stop("'date_end' must be Date whose day is 1.")
  }


  # Prepare raw data
  #rawdata <- readr::read_csv(file, col_names = FALSE, col_types = cols(X1 = col_date(format = "%m/%d/%Y")),
  #                           skip = 2)
  rawdata <- read.csv(file, header = TRUE)
           #skip = 2)
  rawdata <- rawdata[2:(nrow(rawdata) - 1), ] # remove NA rows
  rawdata <- as.data.frame(rawdata)
  colnames(rawdata) <- c("date", colnames(rawdata))[1:ncol(rawdata)]

  # Import tcode tcodes is an internal data of the R package
  tcode <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 2, 2, 2, 5, 5, 2, 2, 5, 5, 5, 5, 5, 5, 5,
             5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 1, 2, 1, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 2, 6,
             6, 5, 6, 6, 7, 6, 6, 6, 2, 5, 5, 2, 5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 5, 5,
             5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 2, 6, 6, 6, 6, 1)


  # Subfunction transxf: data transformation based on tcodes
  transxf <- function(x, tcode) {
    # Number of observations (including missing values)
    n <- length(x)

    # Value close to zero
    small <- 1e-06

    # Allocate output variable
    y <- rep(NA, n)
    y1 <- rep(NA, n)

    # TRANSFORMATION: Determine case 1-7 by transformation code
    if (tcode == 1) {
      # Case 1 Level (i.e. no transformation): x(t)
      y <- x

    } else if (tcode == 2) {
      # Case 2 First difference: x(t)-x(t-1)
      y[2:n] <- x[2:n] - x[1:(n - 1)]

    } else if (tcode == 3) {
      # case 3 Second difference: (x(t)-x(t-1))-(x(t-1)-x(t-2))
      y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]

    } else if (tcode == 4) {
      # case 4 Natural log: ln(x)
      if (min(x, na.rm = TRUE) > small)
        y <- log(x)

    } else if (tcode == 5) {
      # case 5 First difference of natural log: ln(x)-ln(x-1)
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[2:n] <- x[2:n] - x[1:(n - 1)]
      }

    } else if (tcode == 6) {
      # case 6 Second difference of natural log:
      # (ln(x)-ln(x-1))-(ln(x-1)-ln(x-2))
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
      }

    } else if (tcode == 7) {
      # case 7 First difference of percent change:
      # (x(t)/x(t-1)-1)-(x(t-1)/x(t-2)-1)
      y1[2:n] <- (x[2:n] - x[1:(n - 1)])/x[1:(n - 1)]
      y[3:n] <- y1[3:n] - y1[2:(n - 1)]
    }

    return(y)
  }


  # Transform data
  if (transform) {
    # Apply transformations
    N <- ncol(rawdata)
    data <- rawdata
    data[, 2:N] <- NA

    # Perform transformation using subfunction transxf (see below for
    # details)
    for (i in 2:N) {
      temp <- transxf(rawdata[, i], tcode[i - 1])
      data[, i] <- temp
    }

  } else {
    data <- rawdata
  }


  # Null case of date_start and date_end
  if (is.null(date_start))
    date_start <- as.Date("1959-01-01")
  if (is.null(date_end))
    date_end <- data[, 1][nrow(data)]


  # Subset data
  index_start <- which.max(data[, 1] == date_start)
  index_end <- which.max(data[, 1] == date_end)

  outdata <- data[index_start:index_end, ]
  class(outdata) <- c("data.frame", "fredmd")
  return(outdata)

}



#' @title Loading FRED-QD Data Set
#'
#' @description \code{fredqd} loads the official FRED-QD data set and provides
#' a few tools to manipulate the data set.
#'
#' @import readr
#' @export
#'
#' @param file Either a path to a file, a connection, or literal data (either a single string or a raw vector).
#' @param date_start Date or \code{NULL}, the start date (included) of the data selection.
#' If \code{NULL}, select till the latest data available.
#' @param date_end Date or \code{NULL}, the end date (included) of the data selection.
#' If \code{NULL}, select up to the earliest data available.
#' @param transform logical, indicating Whether or not the FRED-MD data set
#' should be transformed according to the transformation code.
#' @return a subset of the (transformed) FRED-MD data of class \code{fredmd}.
#'
#' @author Yankang (Bennie) Chen <yankang.chen@@yale.edu>
#'
#' @references
#' Michael W. McCracken and Serena Ng (2015), \emph{FRED-MD and FRED-QD: Monthly and Quarterly Databases for Macroeconomic Research}.
#' \url{https://research.stlouisfed.org/econ/mccracken/fred-databases/}
fredqd <- function(file = NULL, date_start = NULL, date_end = NULL, transform = TRUE) {
  if(is.null(file)) file <- "https://files.stlouisfed.org/files/htdocs/fred-md/quarterly/current.csv"
  # Error checking
  if (!is.logical(transform))
    stop("'transform' must be logical.")
  if ((class(date_start) != "Date") && (!is.null(date_start)))
    stop("'date_start' must be Date or NULL.")
  if ((class(date_end) != "Date") && (!is.null(date_end)))
    stop("'date_end' must be Date or NULL.")

  if (class(date_start) == "Date") {
    if (as.numeric(format(date_start, "%d")) != 1)
      stop("'date_start' must be Date whose day is 1.")
    if (!as.numeric(format(date_start, "%m")) %in% c(3,6,9,12))
      stop("'date_start' must be Date whose month is March, June,
           September, or December.")
    if (date_start < as.Date("1959-03-01"))
      stop("'date_start' must be later than 1959-03-01.")
  }

  if (class(date_end) == "Date") {
    if (as.numeric(format(date_end, "%d")) != 1)
      stop("'date_end' must be Date whose day is 1.")
    if (!as.numeric(format(date_end, "%m")) %in% c(3,6,9,12))
      stop("'date_end' must be Date whose month is March, June,
           September, or December.")
  }



  # Prepare raw data
  rawdata <- readr::read_csv(file, col_names = FALSE, col_types = readr::cols(X1 = col_date(format = "%m/%d/%Y")),
                             skip = 3)
  ind_notna <- min(which(is.na(rawdata[,1]))) - 1
  rawdata <- rawdata[1:ind_notna, ] # remove NA rows
  rawdata <- as.data.frame(rawdata)

  attrdata <- read.csv(file, header = FALSE, nrows = 3)
  header <- c("date", unlist(attrdata[1,2:ncol(attrdata)]))
  colnames(rawdata) <- header


  # Import tcode tcodes is an internal data of the R package
  tcode <- unlist(attrdata[3,2:ncol(attrdata)])


  # Subfunction transxf: data transformation based on tcodes
  transxf <- function(x, tcode) {
    # Number of observations (including missing values)
    n <- length(x)

    # Value close to zero
    small <- 1e-06

    # Allocate output variable
    y <- rep(NA, n)
    y1 <- rep(NA, n)

    # TRANSFORMATION: Determine case 1-7 by transformation code
    if (tcode == 1) {
      # Case 1 Level (i.e. no transformation): x(t)
      y <- x

    } else if (tcode == 2) {
      # Case 2 First difference: x(t)-x(t-1)
      y[2:n] <- x[2:n] - x[1:(n - 1)]

    } else if (tcode == 3) {
      # case 3 Second difference: (x(t)-x(t-1))-(x(t-1)-x(t-2))
      y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]

    } else if (tcode == 4) {
      # case 4 Natural log: ln(x)
      if (min(x, na.rm = TRUE) > small)
        y <- log(x)

    } else if (tcode == 5) {
      # case 5 First difference of natural log: ln(x)-ln(x-1)
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[2:n] <- x[2:n] - x[1:(n - 1)]
      }

    } else if (tcode == 6) {
      # case 6 Second difference of natural log:
      # (ln(x)-ln(x-1))-(ln(x-1)-ln(x-2))
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
      }

    } else if (tcode == 7) {
      # case 7 First difference of percent change:
      # (x(t)/x(t-1)-1)-(x(t-1)/x(t-2)-1)
      y1[2:n] <- (x[2:n] - x[1:(n - 1)])/x[1:(n - 1)]
      y[3:n] <- y1[3:n] - y1[2:(n - 1)]
    }

    return(y)
  }


  # Transform data
  if (transform) {
    # Apply transformations
    N <- ncol(rawdata)
    data <- rawdata
    data[, 2:N] <- NA

    # Perform transformation using subfunction transxf (see below for
    # details)
    for (i in 2:N) {
      temp <- transxf(rawdata[, i], tcode[i - 1])
      data[, i] <- temp
    }

  } else {
    data <- rawdata
  }


  # Null case of date_start and date_end
  if (is.null(date_start))
    date_start <- as.Date("1959-03-01")
  if (is.null(date_end))
    date_end <- data[, 1][nrow(data)]


  # Subset data
  index_start <- which.max(data[, 1] == date_start)
  index_end <- which.max(data[, 1] == date_end)

  outdata <- data[index_start:index_end, ]
  class(outdata) <- c("data.frame", "fredqd")
  return(outdata)

}


















# transmap <- function(x){
#   out1 <- x[-1,-1]
#   trans_vec <- x[1,-1]
#   for (trans in 1:length(trans_vec)) {
#     if( trans_vec[trans] == 2) out1[,trans] <- c(NA,diff(out1[,trans]))
#     if( trans_vec[trans] == 3) out1[,trans] <- c(NA,NA,diff(out1[,trans], differences =  2))
#     if( trans_vec[trans] == 4) out1[,trans] <- log(out1[,trans])
#     if( trans_vec[trans] == 5) out1[,trans] <- c(NA,diff( log(out1[,trans]) ))
#     if( trans_vec[trans] == 6) out1[,trans] <- c(NA,NA,diff( log(out1[,trans]), differences = 2 ))
#     if( trans_vec[trans] == 7) out1[,trans] <- c(NA, NA,diff(out1[-1,trans]/(out1[-nrow(out1),trans]) ))
#   }
#   out <-  data.frame(date = x[-1,1] ,out1)
#   return(out)
# }




#' @title Download nowcasting data
#'
#' @description Downloads monthly and quarterly data from the FRED site
#'
#' @param m.file Argument to \code{fredmd}. Either a path to a file, a connection, or literal data (either a single string or a raw vector)
#' @param q.file Argument to \code{fredqd}.
#' @param y.name response variable
#' @param date_start Date or \code{NULL}, the start date (included) of the data selection.
#' If \code{NULL}, select till the latest data available.
#' @param date_end Date or \code{NULL}, the end date (included) of the data selection.
#' If \code{NULL}, select up to the earliest data available.
#' @param transform logical, indicating Whether or not the FRED-MD data set
#' should be transformed according to the transformation code.
#' @param na.rm remove rows and colums containing \code{NA}
#' @return List containing \code{ts} objects:
#' \itemize{
#'   \item{\code{fmd}}{ a subset of the (transformed) FRED-MD data of class \code{fredmd}.}
#'   \item{\code{fqd}}{ a subset of the (transformed) FRED-QD data of class \code{fredmd}.}
#'   \item{\code{y}}{ response variable}
#' }
#' .
#' @export
#'
#' @examples nowcasting_data <- get.data()
get.data <- function(m.file = NULL, q.file = NULL, y.name = "GDPC1", date_start = NULL, date_end = NULL, transform = TRUE, na.rm = TRUE){
  #FRED-MD monthly data
  fmd <- fredmd(m.file, date_start = date_start, date_end = date_end, transform = transform)
  #FRED-QD quarterly data
  fqd <- fredqd(q.file, date_start = date_start, date_end = date_end, transform = transform)

  if(is.null(date_start)) date_start <- "1959-01-01"
  startdate <- c(substr(date_start, 3,4),substr(date_start, 7, 7) )
  fmd.ts <- ts(fmd[,-1], frequency = 12, start = startdate)
  fqd.ts <- ts(fqd[,-1], frequency = 4, start = startdate)
  y <- ts(fqd[,y.name], frequency = 4, start = startdate)

  return(list(fmd = fmd.ts, fqd = fqd.ts, y = y))
}
