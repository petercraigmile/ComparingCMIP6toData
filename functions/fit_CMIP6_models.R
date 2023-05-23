
X <- trend.design.matrix(CMIP6$years)

mnames <- CMIP6$model.names

umodels <- unique(CMIP6$model.names)

group <- sapply(strsplit(umodels, "-"), function (x) x[1])
stub <- sapply(strsplit(umodels, "-"), function (x) paste(x[-1], collapse="-"))


## colors must be run after defining the group
source("../functions/colors.R")


num.models <- length(umodels)

sel.models <- lapply(1:num.models, function (k) which(CMIP6$num==k))

num.runs <- ncol(CMIP6$model.anoms)

the.models <- mclapply(1:num.runs, function (k) {
    fit.trend.ARMA.model(CMIP6$model.anoms[,k], X, 4)
}, mc.cores=7)

trend.ests <- sapply(the.models, function (x) x$est.trend)

beta.hats <- sapply(the.models, function (mm) mm$coef[-(1:4)])



in.CI <- (TS$exc.trends$a <= trend.ests) & (trend.ests <= TS$exc.trends$b)

trend.inside <- sapply(1:ncol(in.CI), function (k) all(in.CI[,k]))






fs <- seq(0, 0.5, length=256)


sdf.ests <- sapply(the.models, function (mm) {
    dB(ar.sdf(fs, mm$coef[1:4], mm$sigma2))
})






in.sdfs.CI <- (TS$exc.sdfs$a <= sdf.ests) & (sdf.ests <= TS$exc.sdfs$b)

sdfs.inside <- sapply(1:ncol(in.sdfs.CI), function (k) all(in.sdfs.CI[,k]))



## Calculate the averages by CMIP6 model, and fit the models

unames <- unique(CMIP6$model.names)

avgs <- sapply(1:length(unames), function (j) {
    
    sel <- CMIP6$model.names == unames[j]

    rowMeans(cbind(CMIP6$model.anoms[,sel]))
})



avgs.the.models <- mclapply(1:ncol(avgs), function (k) {
    fit.trend.ARMA.model(avgs[,k], X, 4)
}, mc.cores=7)

avgs.trend.ests <- sapply(avgs.the.models, function (x) x$est.trend)

avgs.beta.hats <- sapply(avgs.the.models, function (mm) mm$coef[-(1:4)])




avgs.in.CI <- (TS$exc.trends$a <= avgs.trend.ests) & (avgs.trend.ests <= TS$exc.trends$b)

avgs.trend.inside <- sapply(1:ncol(avgs.in.CI), function (k) all(avgs.in.CI[,k]))



avg.sdf.ests <- sapply(avgs.the.models, function (mm) {
    dB(ar.sdf(fs, mm$coef[1:4], mm$sigma2))
})




avg.in.sdfs.CI <- (TS$exc.sdfs$a <= avg.sdf.ests) & (avg.sdf.ests <= TS$exc.sdfs$b)

avg.sdfs.inside <- sapply(1:ncol(avg.in.sdfs.CI), function (k) all(avg.in.sdfs.CI[,k]))
