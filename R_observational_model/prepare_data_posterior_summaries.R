
load("chains/data_products_corr_AR4.RData")

library(excursions)

source("functions/AR.R")
source("../functions/AR_spectra.R")
source("../functions/avg_diff.R")



## Calculate the average warming


post.Y <- simplify2array(dch$Y.chain)

post.avg.diffs <- sapply(1:ncol(post.Y), function (j)
    avg.diff(post.Y[,j], year=dch$years))

save(post.avg.diffs, file="../Derived_Data/post_avg_diffs.RData")


fs <- seq(0, 1/2, length=256)


post.trend <- dch$X %*% simplify2array(dch$beta.chain)

post.mean.trend <- rowMeans(post.trend)

save(post.mean.trend, file="../Derived_Data/post_mean_trend.RData")



simband.post.trend   <- simconf.mc(post.trend, 0.05)

save(simband.post.trend, file="../Derived_Data/simband_post_trend.RData")



post.sdfs <- sapply(1:length(dch$eta.chain), function (k) {
    dB(ar.sdf(fs, AR.pacf.to.ar(to.pacf(dch$eta.chain[[k]])),
              dch$sigma2.chain[[k]])) })


post.mean.sdfs <- rowMeans(post.sdfs)

save(post.mean.sdfs, file="../Derived_Data/post_mean_sdfs.RData")



simband.post.sdfs <- simconf.mc(post.sdfs, 0.05)

save(simband.post.sdfs, file="../Derived_Data/simband_post_sdfs.RData")



