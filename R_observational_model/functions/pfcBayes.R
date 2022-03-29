## ========================================================================
## Copyright 2005--2020, Peter F. Craigmile, All Rights Reserved
## Address comments about this software to pfc@stat.osu.edu.
##
##
## File     : chains.R
## Contains : R code to create and maintain chains of variables
##            suitable for MCMC.
## Version  : 1.01
## Requires : coda R library to be installed
## Updated  : pfc@stat.osu.edu
##
## Note: This set of R functions needs much more documentation.
##       I will do this as time permits.
##
## The use of this software is permitted for academic purposes only,
## provided its use is acknowledged.  Commercial use is not allowed
## without the permission of the author.  This software is made available
## AS IS, and no warranty -- about the software, its performance, or its
## conformity to any specification -- is given or implied.
## ========================================================================


chain <- function (var.name, value, envir=.GlobalEnv) {
    ## ======================================================================
    ## Create a new chain called 'var.name' with initial value 'value'.
    ## ======================================================================

    ## initialize the variable
    assign(var.name, value, envir=envir)
    
    ## initialize the chain, but don't put the initial value in the chain!
    chain <- paste(var.name, ".chain", sep="")
    assign(chain, list(), envir=envir)
}



chain.update <- function (var.name, value, save=TRUE, envir = .GlobalEnv) {
    ## ======================================================================
    ## Update the variables called 'var.name' with 'value'.
    ## If 'save=TRUE', append 'value' to the end of the 'var.name'.chain list
    ## ======================================================================
  
    assign(var.name, value, envir = envir)
    
    ## if we want to append to the chain (when save is TRUE)
    if (save) {
        
        chain <- paste(var.name, ".chain", sep = "")
        
        m <- length(get(chain, envir = envir)) + 1
        
        eval(substitute(a[[m]] <- av,
                        list(a = as.name(chain), av = value, m = m)),
             envir = envir)
    }
}


chain.save <- function (var.name, file, envir=.GlobalEnv) {
    ## ======================================================================
    ## Save the current state in 'var.name' and chain 'var.name'.chain
    ## to the file 'file.
    ## ======================================================================  
        
    the.names <- c(var.name, paste(var.name, ".chain",  sep=""))
  
    save(list=the.names, file=file, envir=envir)
}






get.chain <- function (var.name, index1, index2,
                       start=1, thin=1, transform,
                       envir=.GlobalEnv) {

    chain <- paste(var.name, ".chain", sep="")
    x <- get(chain, envir=envir)
    
    if (missing(index1)) {
  
        y <- unlist(x)
        label <- var.name    
    } else if (missing(index2)) {
        
        y <- unlist(sapply(x, function (z, index1) z[[index1]], index1=index1))
        label <- paste(var.name, "[", index1, "]", sep="")
        
    } else {
        
        y <- unlist(sapply(x, function (z, index1, index2) z[[index1]][[index2]],
                           index1=index1, index2=index2))
        label <- paste(var.name, index1, "[", index2, "]", sep="")
    }
    
    sel <- seq(from=start, to=length(y), by=thin)
    
    if (missing(transform)) {
        
        x <- y[sel]

    } else {
        
        x <- transform(y[sel])
    }
        
    attr(x, "label") <- label
    x
}



run.MCMC <- function (chains, the.vars, max.iter, thin = 1, every = 100,
                      burn.in = FALSE, how.long=TRUE, envir=.GlobalEnv) {

    if (how.long)
        t1 <- proc.time()
    
    for (k in 1:max.iter) {
        
        if (k%%every == 0) {
            
            if (how.long) {
                t2 <- proc.time()
                delta <- round(as.numeric((t2-t1)[3]) * (max.iter-k) / k, 1)
                cat(round(k/max.iter * 100, 1), "% (", delta, "s remaining) ", sep="")
            } else {
                cat(round(k/max.iter * 100, 1), "% ", sep="")
            }
        }
        
        save <- (k%%thin == 0) & (!burn.in)
        
        sapply(the.vars, function(the.var)
            eval(call(paste("update.", the.var, sep = ""),
                      save = save, chains=chains)))
    }
}


## Defined so that is reciprocal of the gamma function in R

dinvgamma <- function (x, shape, rate=1, scale=1/rate, log=FALSE) {

    if (log) {
        
        -rate/x + shape*log(rate) - lgamma(shape) - (shape+1)*log(x)
        
    } else {
        
        exp(-rate/x) * rate^shape / (gamma(shape) * x^(shape+1))
    }
}



rinvgamma <- function (n, shape, rate=1, scale=1/rate) {

  1.0 / rgamma(n=n, shape=shape, rate=rate)
}



MH <- function (ll.new, ll.old) {

    log(runif(1)) < (ll.new - ll.old)
}



rmvnorm.cond.precision <- function (m, P, eigen.P) {
## ======================================================================
## Purpose: Sample from N( P^{-1} m, P^{-1} )
## Assumes: m and P are real-valued.
##          The length of m is equal to the dimension of the square matrix P.
## ======================================================================

  if (missing(eigen.P)) {
    
    eigen.P <- eigen(P, symmetric=TRUE)
  }
  
  ev <- eigen.P$values

  if (!all(ev >= -sqrt(.Machine$double.eps) * abs(ev[1]))) {
    warning("P is numerically not positive definite")
  }
  
  A <- t(eigen.P$vectors) * sqrt(1/ev)
  
  drop(crossprod(A, A %*% m + rnorm(length(m))))
}
