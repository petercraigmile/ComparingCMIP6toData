

to.eta <- function (pacf) {
    ## ======================================================================
    ## Transform from pacf coefficients to eta (parameters on the real line)
    ## ======================================================================

    log( (1+pacf)/(1-pacf ) )
}


to.pacf <- function (eta) {
    ## ======================================================================
    ## Transform from eta (parameters on the real line) to pacf coefs
    ## ======================================================================

    nu <- exp(eta)

    (nu - 1) / (nu + 1)
}


AR.acvs.to.lag.p <- function (ar, sigma2=1) {
  ## ======================================================================
  ## Calculate lags 0 to p of the autocovariance sequence (ACVS) of an
  ## autoregressive, AR(p) process with coefficients 'ar' (of length
  ## p), and innovation variance sigma2.
  ##
  ## Based on an argument for calculating the ACVS of an AR(p) process
  ## from Chapter 3 of Brockwell and Davis (2002).
  ##
  ## pfc@stat.osu.edu
  ## ======================================================================
  
  p  <- length(ar)
  mm <- diag(p+1)

  ## CAREFUL: This code is written to overwrite the matrix 'mm' 
  ##          many times per iteration!
  for (h in 0:p) {
    for (j in 1:p) {
      k <- abs(h-j)+1
      mm[h+1, k] <- mm[h+1, k] - ar[j]
    }
  }

  solve(mm, c(sigma2, rep(0, p)))
}



AR.acvs <- function (lag.max, ar, sigma2=1) {
  ## ======================================================================
  ## Calculate lags 0 to lag.max of the autocovariance sequence (ACVS)
  ## of an AR(p) process with coefficients 'ar', and innovation
  ## variance sigma2.  (Assumes lag.max is positive.)
  ##
  ## pfc@stat.osu.edu
  ## ======================================================================

  acvs.p <- AR.acvs.to.lag.p(ar, sigma2)
  ARMAacf(ar, lag.max=lag.max) * acvs.p[1]
}



AR.pacf.acvs <- function (lag.max, pacf, sigma2=1) {
    ## ======================================================================
    ## Calculate lags 0 to lag.max of the autocovariance sequence (ACVS)
    ## of an AR(p) process with pacf coefficients 'pacf', and innovation
    ## variance sigma2.  (Assumes lag.max is positive.)
    ##
    ## pfc@stat.osu.edu
    ## ======================================================================

    ar <- AR.pacf.to.ar(pacf)
    acvs.p <- AR.acvs.to.lag.p(ar, sigma2)
    ARMAacf(ar, lag.max=lag.max) * acvs.p[1]
}




AR.pacf.to.ar <- function (pacf) {
    ## ======================================================================
    ## For an AR(p) process given only the partial autocorrelation up to lag
    ## p, calculate the phi[p,k] values using the LD recursions
    ##
    ## pfc@stat.osu.edu
    ## ======================================================================

    p <- length(pacf)

    if (p==1) {

        pacf

    } else {
        
        Phi <- matrix(NA, p, p)
        
        diag(Phi) <- pacf
        
        for (j in 2:p) {
            
            ks <- 1:(j-1)
            Phi[j,ks] <- Phi[j-1, ks] - Phi[j,j] * rev(Phi[j-1, ks])
        }
        
        Phi[length(pacf), ]
    }
}


dB <- function (x) {
    10 * log10(x)
}


ar.sdf <- function (freqs, phi, sigma2 = 1, delta.t = 1) 
{
    ws <- -2 * pi * delta.t * freqs
    js <- seq(length(phi))
    reals <- sapply(ws, function(w, js, phi) 1 - sum(phi * cos(js * 
        w)), js = js, phi = phi)
    imags <- sapply(ws, function(w, js, phi) sum(phi * sin(js * 
        w)), js = js, phi = phi)
    (sigma2 * delta.t)/(reals * reals + imags * imags)
}
