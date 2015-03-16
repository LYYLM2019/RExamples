#######################################################################        ## Modified version of 'ugarchroll' in 'rugarch' ##
         ## -- ugarchroll does not accept n.ahead > 1 -- ##
#######################################################################

#######################################################################
             ## A. Changes made in 'ugarchroll' ##
#######################################################################
## 1. Can take values of n.ahead > 1
## 2. Added agrument, 'n.ahead.extract'.
##    n.head.extract -- The nth step ahead forecast to return. 
##     NOTE: The function can return only one forecast. 
## 3. refit.every is fixed to 1 to make n.roll = 0. If refit.every is 
##    greater than one, n.roll becomes greater than 0 in
##    ugarchforecast and the model is not re-fitted for every rolling
##    forecast. 
## 4. Removed VaR computation from the original function.

#######################################################################
    ## B. The function returns a list of 'forecast' and 'model' ##
#######################################################################
## Forecast contains:
## ForecastOn : time on which forecast is made. The fit is on till
##              t = ForecastOn.
## PeriodAhead: indicates n.ahead.extract
## Mu         : forecast of returns
## Sigma      : forecast of conditional sd 
## Realized   : realized returns
## Rest are distribution parameter estimates

## 2. 'model' is a list consisting of  model specification, data,
##     fit etc. 

## NOTE: The changes in the function are indicated by M.

ugarchrollNew <- function (spec, data, n.ahead,
                            n.ahead.extract, # new arg
                            forecast.length = 500, n.start = NULL, 
                            refit.every = 1, # fixed this to one
                            refit.window = c("recursive", "moving"), 
                            window.size = NULL, solver = "hybrid",
                            fit.control = list(), 
                            solver.control = list(),
                            cluster = NULL, keep.coef = TRUE, ...) 
{
    tic = Sys.time()
    
    if (is.null(solver.control$trace)) 
        trace = 0
    else trace = solver.control$trace
    if (is.null(fit.control$stationarity)) 
        fit.control$stationarity = TRUE
    if (is.null(fit.control$fixed.se)) 
        fit.control$fixed.se = FALSE
    if (is.null(fit.control$scale)) 
        fit.control$scale = FALSE
    if (is.null(fit.control$rec.init)) 
        fit.control$rec.init = "all"
    mm = match(names(fit.control), c("stationarity", "fixed.se", 
        "scale", "rec.init"))
    if (any(is.na(mm))) {
        idx = which(is.na(mm))
        enx = NULL
        for (i in 1:length(idx))
            enx = c(enx, names(fit.control)[idx[i]])
        warning(paste(c("unidentified option(s) in fit.control:\n", 
            enx), sep = "", collapse = " "),
                call. = FALSE, domain = NULL)
    }
    refit.window = refit.window[1]
    datanames = names(data)
#======================================================================
    ## M0 (removed .extractdata)
    xdata = data
    data = coredata(xdata)
    index = index(xdata)
    ##    period = xdata$period
#======================================================================
    T = NROW(data)
    modelinc = spec@model$modelinc
    if (modelinc[6] > 0) {
        mexdata = spec@model$modeldata$mexdata
        mex = TRUE
    }
    else {
        mex = FALSE
        mexdata = NULL
    }
    if (modelinc[15] > 0) {
        vexdata = spec@model$modeldata$vexdata
        vex = TRUE
    }
    else {
        vex = FALSE
        vexdata = NULL
    }
#======================================================================    ## M1: Removed n.ahead = 1 condition and replaced it with
    ## refit.every = 1 condition
    if (refit.every > 1) 
        stop("\nugarchroll:--> refit.every > 1 not supported...try again.")
#======================================================================    
    if (is.null(n.start)) {
        if (is.null(forecast.length)) 
            stop("\nugarchroll:--> forecast.length amd n.start are both NULL....try again.")
        n.start = T - forecast.length
    }
    else {
        forecast.length = T - n.start
    }
    if (T <= n.start) 
        stop("\nugarchroll:--> start cannot be greater than length of data")
    s = seq(n.start + refit.every, T, by = refit.every)
    m = length(s)
    out.sample = rep(refit.every, m)
    if (s[m] < T) {
        s = c(s, T)
        m = length(s)
        out.sample = c(out.sample, s[m] - s[m - 1])
    }
    if (refit.window == "recursive") {
        rollind = lapply(1:m, FUN = function(i) 1:s[i])
    }
    else {
        if (!is.null(window.size)) {
            if (window.size < 100) 
                stop("\nugarchroll:--> window size must be greater than 100.")
            rollind = lapply(1:m, FUN = function(i) max(1, (s[i] - 
                window.size - out.sample[i])):s[i])
        }
        else {
            rollind = lapply(1:m, FUN = function(i) (1 + (i - 
                1) * refit.every):s[i])
        }
    }
    distribution = spec@model$modeldesc$distribution
    if (any(distribution == c("snorm", "sstd", "sged", "jsu", 
        "nig", "ghyp", "ghst"))) 
        skewed = TRUE
    else skewed = FALSE
    if (any(distribution == c("std", "sstd", "ged", "sged", "jsu", 
        "nig", "ghyp", "ghst"))) 
        shaped = TRUE
    else shaped = FALSE
    if (any(distribution == c("ghyp"))) 
        ghyp = TRUE
    else ghyp = FALSE
    if (!is.null(cluster)) {
        clusterEvalQ(cl = cluster, library(rugarch))
        clusterExport(cluster, c("data", "index", "s", "refit.every", 
            "keep.coef", "shaped", "skewed", "ghyp", "rollind", 
            "spec", "out.sample", "mex", "vex", "solver", "solver.control", 
            "fit.control"), envir = environment())
        if (mex) 
            clusterExport(cluster, c("mexdata"), envir = environment())
        if (vex) 
            clusterExport(cluster, c("vexdata"), envir = environment())
        tmp = parLapply(cl = cluster, 1:m, fun = function(i) {
            if (mex) 
                spec@model$modeldata$mexdata = mexdata[rollind[[i]], 
                  , drop = FALSE]
            if (vex) 
                spec@model$modeldata$vexdata = vexdata[rollind[[i]], 
                  , drop = FALSE]
            fit = try(ugarchfit(spec, xts::xts(data[rollind[[i]]], 
                index[rollind[[i]]]), out.sample = out.sample[i], 
                solver = solver, solver.control = solver.control, 
                fit.control = fit.control), silent = TRUE)
            if (inherits(fit, "try-error") || convergence(fit) != 
                0 || is.null(fit@fit$cvar)) {
                ans = list(y = NA, cf = NA, converge = FALSE, 
                  loglik = NA)
            }
            else {
                if (mex) 
                  fmex = tail(mexdata[rollind[[i]], , drop = FALSE], 
                    out.sample[i])
                else fmex = NULL
                if (vex) 
                  fvex = tail(vexdata[rollind[[i]], , drop = FALSE], 
                    out.sample[i])
                else fvex = NULL
#======================================================================
                ## M2
                f = ugarchforecast(fit, n.ahead = n.ahead, # here
                    n.roll = out.sample[i] - 1,
                    external.forecasts = list(mregfor = fmex, 
                  vregfor = fvex))

                ## M3
                sig = sigma(f)[n.ahead.extract,]
                ret = fitted(f)[n.ahead.extract,]
#======================================================================
                if (shaped) 
                  shp = rep(coef(fit)["shape"], out.sample[i])
                else shp = rep(0, out.sample[i])
                if (skewed) 
                  skw = rep(coef(fit)["skew"], out.sample[i])
                else skw = rep(0, out.sample[i])
                if (ghyp) 
                  shpgig = rep(coef(fit)["ghlambda"], out.sample[i])
                else shpgig = rep(0, out.sample[i])
                rlz = tail(data[rollind[[i]]], out.sample[i])
                
#======================================================================
                ## M4
                forOn <- colnames(sigma(f)) # forecast made on
                period.ahead <- rownames(sigma(f))[n.ahead.extract]
                y = as.data.frame(forOn, period.ahead,
                    ret, sig, skw, shp, shpgig, rlz)
            ## Since n.roll = 0, no longer required
            ## rownames(y) =
            ## tail(as.character(index[rollind[[i]]]), out.sample[i])
            colnames(y) = c("ForecastOn", "PeriodAhead",
                        "Mu", "Sigma", "Skew", "Shape", 
                        "Shape(GIG)", "Realized")
#======================================================================
            if (keep.coef) 
                  cf = fit@fit$robust.matcoef
                else cf = NA
                ans = list(y = y, cf = cf, converge = TRUE,
                    loglik = likelihood(fit))
            }
            return(ans)
        })
    }
    else {
        tmp = lapply(as.list(1:m), FUN = function(i) {
            if (mex) 
                spec@model$modeldata$mexdata = mexdata[rollind[[i]], 
                  , drop = FALSE]
            if (vex) 
                spec@model$modeldata$vexdata = vexdata[rollind[[i]], 
                  , drop = FALSE]

            fit = try(ugarchfit(spec, xts(data[rollind[[i]]], 
                index[rollind[[i]]]), out.sample = out.sample[i], 
                solver = solver, solver.control = solver.control, 
                fit.control = fit.control), silent = TRUE)
            if (inherits(fit, "try-error") || convergence(fit) != 
                0 || is.null(fit@fit$cvar)) {
                ans = list(y = NA, cf = NA, converge = FALSE, 
                  loglik = NA)
            }
            else {
                if (mex) 
                  fmex = tail(mexdata[rollind[[i]], , drop = FALSE], 
                    out.sample[i])
                else fmex = NULL
                if (vex) 
                  fvex = tail(vexdata[rollind[[i]], , drop = FALSE], 
                    out.sample[i])
                else fvex = NULL

#======================================================================                ## M5
                f = ugarchforecast(fit, n.ahead = n.ahead, #here
                    n.roll = out.sample[i] - 1,
                    external.forecasts = list(mregfor = fmex, 
                  vregfor = fvex))

                ## M6
                sig = sigma(f)[n.ahead.extract,]
                ret = fitted(f)[n.ahead.extract,]
#======================================================================
                if (shaped) 
                  shp = rep(coef(fit)["shape"], out.sample[i])
                else shp = rep(0, out.sample[i])
                if (skewed) 
                  skw = rep(coef(fit)["skew"], out.sample[i])
                else skw = rep(0, out.sample[i])
                if (ghyp) 
                  shpgig = rep(coef(fit)["ghlambda"], out.sample[i])
                else shpgig = rep(0, out.sample[i])
                rlz = tail(data[rollind[[i]]], out.sample[i])

#======================================================================                ## M7
                forOn <- colnames(sigma(f)) # forecast made on
                period.ahead <- rownames(sigma(f))[n.ahead.extract]
                y = data.frame(forOn, period.ahead,
                    ret, sig, skw, shp, shpgig, rlz)
            ## Since n.roll = 0, no longer required
            ## rownames(y) =
            ## tail(as.character(index[rollind[[i]]]), out.sample[i])
            colnames(y) = c("ForecastOn", "PeriodAhead",
                        "Mu", "Sigma", "Skew", "Shape", 
                        "Shape(GIG)", "Realized")
#======================================================================                
            if (keep.coef) 
                cf = fit@fit$robust.matcoef
            else cf = NA
            ans = list(y = y, cf = cf, converge = TRUE,
                loglik = likelihood(fit))
        }
            return(ans)
        })
    }
    conv = sapply(tmp, FUN = function(x) x$converge)
if (any(!conv)) {
    warning("\nnon-converged estimation windows present...resubsmit object with different solver parameters...")
#======================================================================
     ## M8: Removed VaR calculation
#======================================================================        noncidx = which(!conv)
        model = list()
        model$spec = spec
        model$data = data
        model$index = index
        #model$period = period
        model$datanames = datanames
        model$n.ahead = n.ahead
        model$forecast.length = forecast.length
        model$n.start = n.start
        model$n.refits = m
        model$refit.every = refit.every
        model$refit.window = refit.window
        model$window.size = window.size
        model$keep.coef = keep.coef
        model$noncidx = noncidx
        model$rollind = rollind
        model$out.sample = out.sample
        model$distribution = distribution
        model$n.ahead.extract = n.ahead.extract
        forecast = tmp
        toc = Sys.time() - tic
        model$elapsed = toc
        ans = list(model = model, forecast = forecast)
        return(ans)
    }
    else {
        noncidx = NULL
        forc = tmp[[1]]$y
        for (i in 2:m) {
            forc = rbind(forc, tmp[[i]]$y)
        }
        cf = vector(mode = "list", length = m)
        for (i in 1:m) {
            cf[[i]]$index = index[tail(rollind[[i]], 1) - out.sample[i]]
            cf[[i]]$coef = tmp[[i]]$cf
        }
        
        model = list()
        model$spec = spec
        model$data = data
        model$index = index
        #model$period = period
        model$n.ahead = n.ahead
        model$forecast.length = forecast.length
        model$n.start = n.start
        model$refit.every = refit.every
        model$n.refits = m
        model$refit.window = refit.window
        model$window.size = window.size
        model$keep.coef = keep.coef
        model$noncidx = noncidx
        model$coef = cf
        model$rollind = rollind
        model$out.sample = out.sample
        model$distribution = distribution
        model$n.ahead.extract = n.ahead.extract
        model$loglik = sapply(tmp, function(x) x$loglik)
        forecast = forc
    }
    toc = Sys.time() - tic
    model$elapsed = toc
    ans = list( model = model, forecast = forecast)
    return(ans)
}
