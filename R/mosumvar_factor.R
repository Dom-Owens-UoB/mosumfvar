
#' Segment data under a factor model with VAR dynamics
#'
#' @param x matrix of data with series as columns
#' @param center whether to de-mean the input \code{x}
#' @param q Either the number of factors or a string specifying the factor number selection method; possible values are:
#' \itemize{
#'    \item{\code{"ic"}}{ information criteria-based methods of Alessi, Barigozzi & Capasso (2010) when \code{fm.restricted = TRUE} or Hallin and Liška (2007) when \code{fm.restricted = FALSE} modifying Bai and Ng (2002)}
#'    \item{\code{"er"}}{ eigenvalue ratio of Ahn and Horenstein (2013)}
#' }
#' see \link[fnets]{factor.number}.
#' @param order integer VAR model order
#' @param G integer MOSUM bandwidth (or vector, if \code{algo = "ms"}); see reference for default
#' @param method detector, one of \code{"Wald", "Score"}
#' @param estim estimator method, one of \code{"C", "H"}
#' @param var.estim variance estimator method, one of \code{"Local", "Global"}
#' @param alpha Numeric significance level
#' @param criterion location procedure, one of \code{"eps", "eta"}
#' @param nu Numeric location procedure hyperparameter
#' @param do.bootstrap Boolean, determine threshold via bootstrap method
#' @param n.bootstrap Integer; number of bootstrap replicates
#' @param thresh rejection threshold; see reference for default
#' @param do.plot Boolean, return plot
#' @param algo which algorithm to use, one of \code{"mosumvar","univ","ms"}
#' @param rm.cross.terms Boolean, remove cross terms when \code{univ = TRUE}
#' @param global.resids Boolean, use residuals from full VAR model when \code{univ = TRUE}
#'
#' @return List of class \code{mosumfvar}, containing
#' \itemize{
#'   \item{\code{seg}}{ resulting segmentation, a \code{mosumvar} object. See \link[mosumvar]{mosumvar}.}
#'   \item{\code{fm}}{ fitted factor model, a \code{fm} object. See \link[fnets]{fnets.factor.model}.}
#' }
#' @export
#' @importFrom fnets fnets.factor.model
#' @examples mosumfvar(panel$panel, order = 1,  method = "Score", q = 2)
mosumfvar <- function(x,
                      center = TRUE,
                      q = c("ic", "er"),
                      order = NULL,
                      G = NULL, method = c("Score","Wald"), estim = c("C","H"), var.estim = c("Local", "Global"), alpha = 0.05,
                      criterion = c("eps", "eta"), nu = 0.25, do.bootstrap = FALSE, n.bootstrap = 1000, thresh = NULL,
                      do.plot = TRUE, algo = c("mosumvar","univ","ms"), rm.cross.terms = TRUE, global.resids = TRUE) {
  n <- nrow(x); p <- ncol(x)
  ifelse(center, mean.x <- colMeans(x), mean.x <- rep(0, p))
  xx <- x - mean.x
  if (!is.numeric(q)) {
    q.method <- match.arg(q, c("ic", "er"))
    q <- NULL
  } else q.method <- q

  method <- match.arg(method, c("Score","Wald"))
  algo <- match.arg(algo, c("mosumvar","univ","ms"))
  fm <- fnets::fnets.factor.model(t(xx), q = q.method, center = FALSE, fm.restricted = TRUE)
  r <- fm$q

  f <- fm$factors[,1:q, drop = FALSE]
  if(is.null(order)){
    nn <- max(floor(nrow(xx)/20), 50)
    order_ar <- ar(f[1:nn,], order.max = 3)
    order <- max(order_ar$order, 1)
  }
  if (algo == "univ") {
    cp_mosumvar <- mosumvar::mosumvar.uni( x=f, order=order, G=G, method = method, estim =estim,
                                           var.estim = var.estim, alpha = alpha, rm.cross.terms = rm.cross.terms, global.resids = global.resids,
                                           criterion = criterion, nu = nu, do.bootstrap = do.bootstrap, n.bootstrap = n.bootstrap, do.plot = do.plot)
  } else if (algo == "ms") {
    cp_mosumvar <- mosumvar::mosumvar.ms( x=f, order=order, Gset=G, method = method, estim =estim, alpha = alpha, do.plot = do.plot)
  } else if (algo == "mosumvar") {
    cp_mosumvar <- mosumvar::mosumvar( x=f, order=order, G=G, method = method, estim =estim,
                                     var.estim = var.estim, alpha = alpha,
                                     criterion = criterion, nu = nu, do.bootstrap = do.bootstrap, n.bootstrap = n.bootstrap, do.plot = do.plot)
  }
  fm$mean.x <- t(mean.x)
  out <- list(
    seg = cp_mosumvar,
    fm = fm
  )
  attr(out, "class") <- "mosumfvar"
  return(out)
}



#' Fit an autoregressive time series model to the data, using weighted estimators
#'
#' @param x matrix of data with series as columns
#' @param cps integer (vector) of estimated change points
#' @param weight.method String of weight method to use
#' @param ... further arguments to \code{ar}
#'
#' @return \code{ar.weighted} object, see \link[stats]{ar}
#' @export
#'
#' @examples fm <- fnets::fnets.factor.model(t(panel$panel), fm.restricted = TRUE, q = 2)
#' mod <- ar.weighted(fm$factors, cps = 100)
#' predict(mod, fm$factors, n.ahead = 5)
ar.weighted <- function(x, cps = NULL, weight.method = c("linear","exp","robust"), ...){
  weight.method <- match.arg(weight.method, c("linear","exp","robust"))

  x <- as.matrix(x)
  cps <- c(0,cps)
  n <- nrow(x)
  q <- length(cps)

  if(q >= 2){
    w <- get.weights(n, cps, weight.method)
    x <- x * sqrt(w)
    x <- x[w > 0,]
  } else {
    w <- rep(1/n,n)
  }

  out <- ar(x, ...)
  out$weights <- w
  return(out)
}




#' Fit a linear model to the data, using weighted estimators
#'
#' @param y vector of responses
#' @param x matrix of data with series as columns
#' @param intercept add intercept in regression
#' @param cps integer (vector) of estimated change points
#' @param weight.method String of weight method to use
#' @param ... further arguments to \code{lm}
#'
#' @return \code{lm} object, see \link[stats]{lm}
#' @export
#'
#' @examples fm <- fnets::fnets.factor.model(t(panel$panel), fm.restricted = TRUE, q = 2)
#' lm.weighted(panel$gdp, fm$factors, cps = 100)
lm.weighted <- function(y, x, intercept = FALSE, cps = NULL, weight.method = c("linear","exp","robust"), ...){
  weight.method <- match.arg(weight.method, c("linear","exp","robust"))

  cps <- c(0,cps)
  n <- nrow(x)
  q <- length(cps)
  w <- get.weights(n, cps, weight.method)
  if(intercept) {formula <- y ~ x} else {formula <- y ~ x - 1}
  out <- lm(formula, weights = w, ...)
  return(out)
}

#' Get regression weights
#'
#' @param n sample size
#' @param cps integer (vector) of estimated change points
#' @param weight.method String of weight method to use
#'
#' @return weight vector
#' @keywords internal
get.weights <- function(n, cps, weight.method){
  w <- rep(1, n)
  cps <- cps[cps<=n]
  q <- length(cps)
  if(q>=2){
    if(weight.method == "linear"){
      w[0:cps[q-1]] <- 0
      w[(cps[q-1]+1):cps[q]] <- (cps[q-1]+1):cps[q] / (cps[q] - cps[q-1]+1)
    }
    if(weight.method == "exp") w <- exp(seq.int(n) )
    if (weight.method == "robust"){
      w[1:cps[q-1]] <- 0
      n.size <- cps[q] - cps[q-1]
      w[(cps[q-1] +1):(cps[q]-1)] <- log(1 - seq.int(n.size)/n.size )/log(1 - (n.size-1)/n.size)
      w[cps[q]] <- max(w[cps[q]-1],0)
    }
  } else print("Too few change points for weighting; returning equal weights")

  w <- w * n/ sum(w)
  return(w)
}

