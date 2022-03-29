
area.weighted.average <- function (temp.array, areas, cores=1) {

    if (cores==1) {
        the.fun <- sapply
    } else {
        the.fun <- mclapply
    }
    
    out <- the.fun(1:dim(temp.array)[[3]], function (l) {
        
        y <- as.numeric(temp.array[,,l])
        
        sel <- !is.na(y)
        
        a <- as.numeric(areas)[sel]
        
        z <- y[sel]
        
        wt <- a/sum(a)
        
        wmean     <- sum(wt * z)
        wmean.var <- sum(wt * (z - wmean)^2) * sum(wt^2)
        
        c(wmean, wmean.var)
    })

    simplify2array(out)
}


