#' Simulate from a piecewise stationary factor model with VAR dynamics
#'
#' @param n sample size
#' @param p number of series
#' @param r factor number
#' @param order VAR order
#' @param cps change points
#' @param signal size of parameters
#' @param error.dist error distribution for VAR and idiosyncratic errors, one of \code{"normal", "t", "garch"}
#' @param P1 see \link[mosumvar]{VAR.sim}
#' @param Q1 see \link[mosumvar]{VAR.sim}
#' @param df see \link[mosumvar]{VAR.sim}
#'
#' @return List containing
#' \itemize{
#'     \item{\code{x}}{ observed series}
#'     \item{\code{f}}{ factor series}
#'     \item{\code{e}}{ error series}
#'     \item{\code{lam}}{ factor loadings}
#'     \item{\code{cps}}{ change points}
#' }
#' @export
#'
#' @examples data <- fvar.sim(500, cps = 200)
fvar.sim <- function (n, p = 100, q = 2, order = 1, cps = c(), signal = 0.7,
                      error.dist = c("normal", "t", "garch"), P1 = NULL, Q1 = NULL, df = 3){
  error.dist <- match.arg(error.dist, c("normal", "t", "garch"))
  mu = rep(0, q)
  Sigma = diag(1,q)
  if(error.dist == "garch"){
    if(is.null(P1)) P1 <- Sigma
    if(is.null(Q1)) Q1 <- Sigma
  }
  cps <- c(0, cps, n)

  lam <- matrix(runif(q*p, .2,.8), q, p)


  #var params
  A1 <- matrix(-0.1, nrow = q, ncol = q)
  diag(A1) <- 0.7
  A1 <- signal * A1 / (order*norm(A1, "F")^2)
  A2 <- (-1) * A1
  if(order == 1){
    A1list <- A1
    A2list <- A2
  } else {
    A2list <- A1list <- list()
    for (ii in 1:order) {
      A1list[[ii]] <- A1
      A2list[[ii]] <- A2
    }
  }

  #generate data
  f <- matrix(0, nrow = n, ncol = q)
  n.cps <- length(cps) - 1
  for (ii in 1:n.cps) {
    if(ii%%2 == 0)  coeffs <- A1list else coeffs <- A2list
    n_ii <- cps[ii+1]-cps[ii]
    f[(cps[ii]+1):cps[ii+1],] <- mosumvar::VAR.sim(n_ii, mu, Sigma, coeffs, error.dist, P1, Q1, df)
  }
  if(error.dist == "normal"){
    e <- matrix(rnorm(n*p),n,p)
  } else if(error.dist == "t"){
    e <- matrix(rt(n*p, df),n,p) * sqrt(df/(df-2))
  } else if(error.dist == "garch"){
    Sigma <- diag(1, p)
    P1 <- Q1 <- Sigma/2
    e <- z <- matrix(rnorm(n*p),n,p)
    Sig_t <- Sigma
    e[1,] <- z[1,] %*% chol(Sigma)
    for (tt in 2:n){
      Sig_t <- Sigma + P1 %*% outer(z[tt-1,], z[tt-1,]) %*% t(P1) + Q1 %*% Sig_t  %*% t(Q1)
      e[tt,] <- z[tt,] %*% t(chol(Sig_t))
      Sig_t1 <- Sig_t
    }
  }

  x <- f %*% lam + e
  out <- list(x = x, f = f, e = e, lam = lam, cps = cps)
  return(out)
}
# fvar.sim(500, cps = 200)
