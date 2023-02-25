

plot.CI <- function (x, y,
                     lower=apply(y, 1, quantile, p=alpha/2),
                     upper=apply(y, 1, quantile, p=1-alpha/2),
                     alpha=0.05,
                     col="gray70", ...) {

    polygon(c(x, rev(x)), c(lower, rev(upper)), border=NA, col=col, ...)
}

