


trend.design.matrix <- function (years, K=8) {

    cbind(1, bs(years, K))
}

fit.trend.ARMA.model <- function (y, X, P) {

    model <- arima(y, order=c(P,0,0), xreg=X, include.mean=FALSE, method="ML")

    model$order <- P

    if (model$order==0) {

        model$phi <- NA
        model$beta <- model$coef
    } else {

        js <- 1:model$order
        model$phi <- model$coef[js]
        model$beta <- model$coef[-js]
    }
    
    model$est.trend <- drop(X %*% model$beta)

    model
}


AIC.to.AICC <- function (aic, n, npars) {
  ## ======================================================================
  ## Purpose : Transforms the AIC value to the AICC value.
  ## ======================================================================
  
  aic - 2 * npars * ( 1 - n/(n-1-npars))
}


select.trend.ARp.model <- function (y, X, max.P=8, CORES=7) {
    
    ARs <- mclapply(0:max.P, function (p) {
        arima(y, c(p,0,0), xreg=X, include.mean=FALSE, method="ML")
    }, mc.cores=CORES)

    AICs <- sapply(ARs, AIC)

    npars <- (0:max.P) + 1 + ncol(X)

    AICCs <- AIC.to.AICC(AICs, length(y), npars)

    ##print(AICCs)
    
    order <- order(AICCs)[1]

    model <- ARs[[order]]
    model$order <- order-1

    if (model$order==0) {

        model$phi <- NA
        model$beta <- model$coef
    } else {

        js <- 1:model$order
        model$phi <- model$coef[js]
        model$beta <- model$coef[-js]
    }
    
    model$est.trend <- drop(X %*% model$beta)

    model$AICC <- AICCs[order]

    model
}
    
