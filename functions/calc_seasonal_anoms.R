

calc.seasonal.anoms <- function (tas, date, first, last) {

    year  <- as.numeric(substr(date, 1, 4))

    time.in.range <- year >= first & year <= last
    
    y <- tas[,,time.in.range]

    date.in.range <- date[time.in.range]

    month <- as.numeric(substr(date.in.range, 6, 7))
    
    the.dims <- dim(y)
    D1 <- the.dims[1]

    month.ind <- lapply(1:12, function (m) month==m)

    for (j in 1:D1) {
        
        if (j%%50==0) cat(floor(j/D1*100), "% ", sep="")
        
        for (k in 1:the.dims[2]) {
            
            z <- y[j,k,]
            
            for (m in 1:12) {

                w <- z[month.ind[[m]]]
                z[month.ind[[m]]] <- w - mean(w)
            }

            y[j,k,] <- z
        }
    }
    
    list(tas=y, date=date.in.range)
}


