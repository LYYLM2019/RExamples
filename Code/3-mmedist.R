#==========================================================
# purpose: attempt to solve the issue here:
# http://stackoverflow.com/questions/28010329/estimate-parameters-of-frechet-distribution-using-mmedist-or-fitdistwith-mme-e
# date: 18th january 2014
#==========================================================

rm(list = ls())

#==========================================================
# redefine the mmedist function
#==========================================================
foo_mmedist = function (data, distr, order, memp, start = NULL, fix.arg = NULL, 
                        optim.method = "default", lower = -Inf, upper = Inf, custom.optim = NULL, 
                        ...) 
{
  if (!is.character(distr)) 
    stop("distr must be a character string naming a distribution")
  else distname <- distr
  if (is.element(distname, c("norm", "lnorm", "pois", "exp", 
                             "gamma", "nbinom", "geom", "beta", "unif", "logis"))) 
    meth <- "closed formula"
  else meth <- optim.method
  mdistname <- paste("m", distname, sep = "")
  ddistname <- paste("d", distname, sep = "")
  if (meth != "closed formula") {
    if (!exists(mdistname, mode = "function")) 
      stop(paste("The moment function must be defined."))
  }
  if (!(is.numeric(data) & length(data) > 1)) 
    stop("data must be a numeric vector of length greater than 1.")
  if (meth == "closed formula") {
    n <- length(data)
    m <- mean(data)
    v <- (n - 1)/n * var(data)
    if (!is.null(fix.arg)) 
      warnings("argument fix.arg cannot be used when a closed formula is used")
    if (!(is.vector(data) & is.numeric(data) & length(data) > 
            1)) 
      stop("data must be a numeric vector of length greater than 1")
    if (distname == "norm") {
      estimate <- c(mean = m, sd = sqrt(v))
      order <- 1:2
    }
    if (distname == "lnorm") {
      if (any(data <= 0)) 
        stop("values must be positive to fit a lognormal distribution")
      sd2 <- log(1 + v/m^2)
      estimate <- c(meanlog = log(m) - sd2/2, sdlog = sqrt(sd2))
      order <- 1:2
    }
    if (distname == "pois") {
      estimate <- c(lambda = m)
      order <- 1
    }
    if (distname == "exp") {
      estimate <- c(rate = 1/m)
      order <- 1
    }
    if (distname == "gamma") {
      shape <- m^2/v
      rate <- m/v
      estimate <- c(shape = shape, rate = rate)
      order <- 1:2
    }
    if (distname == "nbinom") {
      size <- if (v > m) 
        m^2/(v - m)
      else NaN
      estimate <- c(size = size, mu = m)
      order <- 1:2
    }
    if (distname == "geom") {
      prob <- if (m > 0) 
        1/(1 + m)
      else NaN
      estimate <- c(prob = prob)
      order <- 1
    }
    if (distname == "beta") {
      if (any(data < 0) | any(data > 1)) 
        stop("values must be in [0-1] to fit a beta distribution")
      aux <- m * (1 - m)/v - 1
      shape1 <- m * aux
      shape2 <- (1 - m) * aux
      estimate <- c(shape1 = shape1, shape2 = shape2)
      order <- 1:2
    }
    if (distname == "unif") {
      min1 <- m - sqrt(3 * v)
      max1 <- m + sqrt(3 * v)
      estimate <- c(min1, max1)
      order <- 1:2
    }
    if (distname == "logis") {
      scale <- sqrt(3 * v)/pi
      estimate <- c(location = m, scale = scale)
      order <- 1:2
    }
    res <- list(estimate = estimate, convergence = 0, order = order, 
                memp = NULL)
  }
  else {
    if (length(start) != length(order)) 
      stop("wrong dimension for the moment order to match.")
    if (!exists(as.character(expression(memp)), mode = "function")) 
      stop("the empirical moment function must be defined.")
    vstart <- unlist(start)
    vfix.arg <- unlist(fix.arg)
    argmdistname <- names(formals(mdistname))
    m <- match(names(start), argmdistname)
    mfix <- match(names(vfix.arg), argmdistname)
    if (any(is.na(m)) || length(m) == 0) 
      stop("'start' must specify names which are arguments to 'distr'")
    if (any(is.na(mfix))) 
      stop("'fix.arg' must specify names which are arguments to 'distr'")
    minter <- match(names(start), names(fix.arg))
    if (any(!is.na(minter))) 
      stop("a distribution parameter cannot be specified both in 'start' and 'fix.arg'")
    DIFF2 <- function(par, fix.arg, order, obs, mdistnam, 
                      memp) {
      momtheo <- do.call(mdistnam, c(as.list(order), as.list(par), 
                                     as.list(fix.arg)))
      momemp <- as.numeric(memp(obs, order))
      (momemp - momtheo)^2
    }
    fnobj <- function(par, fix.arg, obs, mdistnam, memp) sum(sapply(order, 
                                                                    function(o) DIFF2(par, fix.arg, o, obs, mdistnam, 
                                                                                      memp)))
    if (optim.method == "default") {
      if (is.infinite(lower) && is.infinite(upper)) {
        if (length(vstart) > 1) 
          meth <- "Nelder-Mead"
        else meth <- "BFGS"
      }
      else meth <- "L-BFGS-B"
    }
    else meth <- optim.method
    cens <- FALSE
    if (is.null(custom.optim)) {
      if (!cens) 
        opttryerror <- try(opt <- optim(par = vstart, 
                                        fn = fnobj, fix.arg = fix.arg, obs = data, 
                                        mdistnam = mdistname, memp = memp, hessian = TRUE, 
                                        method = meth, lower = lower, upper = upper, 
                                        ...), silent = FALSE)
      else stop("Moment matching estimation for censored data is not yet available.")
      if (inherits(opttryerror, "try-error")) {
        warnings("The function optim encountered an error and stopped")
        print(opttryerror)
        return(list(estimate = rep(NA, length(vstart)), 
                    convergence = 100, value = NA, hessian = NA))
      }
      if (opt$convergence > 0) {
        warnings("The function optim failed to converge, with the error code ", 
                 opt$convergence)
        return(list(estimate = rep(NA, length(vstart)), 
                    convergence = opt$convergence, value = NA, 
                    hessian = NA))
      }
      res <- list(estimate = opt$par, convergence = opt$convergence, 
                  value = opt$value, hessian = opt$hessian, order = order, 
                  optim.function = "optim", memp = memp)
    }
    else {
      if (!cens) 
        opttryerror <- try(opt <- custom.optim(fn = fnobj, 
                                               fix.arg = fix.arg, obs = data, mdistnam = mdistname, 
                                               memp = memp, par = vstart, ...), silent = TRUE)
      else stop("Moment matching estimation for censored data is not yet available.")
      if (inherits(opttryerror, "try-error")) {
        warnings("The customized optimization function encountered an error and stopped")
        print(opttryerror)
        return(list(estimate = rep(NA, length(vstart)), 
                    convergence = 100, value = NA, hessian = NA))
      }
      if (opt$convergence > 0) {
        warnings("The customized optimization function failed to converge, with the error code ", 
                 opt$convergence)
        return(list(estimate = rep(NA, length(vstart)), 
                    convergence = opt$convergence, value = NA, 
                    hessian = NA))
      }
      res <- list(estimate = opt$par, convergence = opt$convergence, 
                  value = opt$value, hessian = opt$hessian, order = order, 
                  optim.function = custom.optim, memp = memp)
    }
  }
  loglik <- function(par, fix.arg, obs, ddistnam) {
    sum(log(do.call(ddistnam, c(list(obs), as.list(par), 
                                as.list(fix.arg)))))
  }
  if (exists(ddistname)) 
    loglik <- loglik(res$estimate, fix.arg, data, ddistname)
  else loglik <- NULL
  res <- c(res, fix.arg = fix.arg)
  return(c(res, list(loglik = loglik, method = meth)))
}

#==========================================================

# values
n = 100
scale = 1
shape = 3

# simulate a sample
data_fre = rinvweibull(n, shape, scale)

# estimating the parameters
para_lm = foo_mmedist(data_fre, "invweibull",
                  start= c(shape=5,scale=2), order=c(1, 2), memp = moment)

para_lm$par