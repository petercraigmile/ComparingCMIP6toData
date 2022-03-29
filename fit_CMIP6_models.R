
X <- trend.design.matrix(CMIP6$years)

mnames <- CMIP6$model.names

umodels <- unique(CMIP6$model.names)

group <- sapply(strsplit(umodels, "-"), function (x) x[1])
stub <- sapply(strsplit(umodels, "-"), function (x) paste(x[-1], collapse="-"))

source("functions/colors.R")



num.models <- length(umodels)

sel.models <- lapply(1:num.models, function (k) which(CMIP6$num==k))

num.runs <- ncol(CMIP6$model.anoms)

the.models <- mclapply(1:num.runs, function (k) {
    fit.trend.ARMA.model(CMIP6$model.anoms[,k], X, 4)
}, mc.cores=7)

trend.ests <- sapply(the.models, function (x) x$est.trend)

beta.hats <- sapply(the.models, function (mm) mm$coef[-(1:4)])



in.CI <- (simband.post.trend$a <= trend.ests) & (trend.ests <= simband.post.trend$b)

trend.inside <- sapply(1:ncol(in.CI), function (k) all(in.CI[,k]))




fs <- seq(0, 0.5, length=256)


sdf.ests <- sapply(the.models, function (mm) {
    dB(ar.sdf(fs, mm$coef[1:4], mm$sigma2))
})




in.sdfs.CI <- (simband.post.sdfs$a <= sdf.ests) & (sdf.ests <= simband.post.sdfs$b)

sdfs.inside <- sapply(1:ncol(in.sdfs.CI), function (k) all(in.sdfs.CI[,k]))


