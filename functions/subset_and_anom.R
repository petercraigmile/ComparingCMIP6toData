
## ======================================================================
## When we subset the data and create the anomalies we take away the mean
## from the entire period of study (1880-2020 is the default).
## =====================================================================

subset.and.anom <- function (years, the.mean, the.vars,
                             first.year=1880, last.year=2020) {

    sel <- years>=first.year & years<=last.year

    the.mean <- the.mean[sel]
    the.mean <- the.mean - mean(the.mean, na.rm=TRUE)

    years    <- years[sel]

    if (!missing(the.vars)) {
        
        the.vars <- the.vars[sel]
    } else {

        the.vars <- NULL
    }

    list(years=years, the.mean=the.mean, the.vars=the.vars)
}

