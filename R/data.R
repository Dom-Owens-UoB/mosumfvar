##panel_data

#' Macro and financial data for nowcasting
#'
#' Data from NYFED and FRED-MD. Stationarity transforms have been applied as per original sources, other than GDP which has quarter-within-year smoothing.
#'
#' @docType data
#'
#' @usage data(panel)
#'
#' @format list with panel and gdp components from May 2004 - May 2021
#'
#' @keywords datasets
#'
#' @references Federal Reserve Bank of New York, Nowcasting Report,
#' (\href{https://www.newyorkfed.org/research/policy/nowcast.html}{NYFED})
#'
#' Michael W. McCracken and Serena Ng,FRED-MD: A Monthly Database for Macroeconomic Research,
#' (\href{https://research.stlouisfed.org/wp/more/2015-012}{FRED-MD})
#'
#' @examples
#' data(panel)
#' panelx <- panel$panel
#' gdp <- panel$gdp
"panel"
