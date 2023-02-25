

outer.x.axis <- function (num, label, subset) {

    mult <- 1/num

    ks <- 0:(num-1)
    if (!missing(subset))
        ks <- ks[subset]
    
    for (k in ks) {
        mtext(label,
              at=0.5*mult+mult*k, side=1, outer=TRUE, line=0, cex=0.6)
    }    
}



outer.y.axis <- function (num, label, subset) {

    mult <- 1/num

    ks <- 0:(num-1)
    if (!missing(subset))
        ks <- ks[subset]

    
    for (k in 0:(num-1)) {
        mtext(label,
              at=0.5*mult+mult*k, side=2, outer=TRUE, line=0, cex=0.6)
    }    
}

