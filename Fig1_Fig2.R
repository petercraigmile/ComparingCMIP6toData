

load("Derived_Data/CMIP6_tas_historical.RData")
load("Derived_Data/data_products_no_JMA.RData")

load("Derived_Data/post_mean_trend.RData")
load("Derived_Data/simband_post_trend.RData")
load("Derived_Data/post_mean_sdfs.RData")
load("Derived_Data/simband_post_sdfs.RData")

source("functions/AR_spectra.R")
source("functions/plot_CI.R")


cylim <- c(-1, 1.5)

pdf(file="figures/Fig1_CMIP6_global_tas.pdf", width=4.05, height=2)
par(mfrow=c(1,1), cex=0.6, mar=c(3,3,1,0.5), mgp=c(1.8,0.5,0), bty="L")

matplot(CMIP6$years, CMIP6$model.anoms, type="l", ylim=cylim,
        col="gray", lty=1,
        xlab="year",
        ylab=expression(paste("Global temperature anomaly (",degree,"C)" )))

for (j in 1:4) {
    lines(data.prods$years, data.prods$global.anoms[,j], lwd=1, col="blue")
}

dev.off()



fs <- seq(0, 1/2, length=256)


pdf(file="figures/Fig2_trends_sdf_AR4.pdf", width=4.05, height=4)
par(mfrow=c(2,1), cex=0.6, mar=c(3,3,1,0.5), mgp=c(1.8,0.5,0), bty="L")

plot(CMIP6$year, post.mean.trend, type="n", ylim=cylim,
     xlab="Year", ylab=expression(paste("Posterior Trend (",degree,"C)")))

plot.CI(CMIP6$year,
        lower=simband.post.trend$a,
        upper=simband.post.trend$b,
        col="lightgray")

lines(CMIP6$year, post.mean.trend)

mtext("(a)", side=3, line=0, cex=0.7)


plot(fs, post.mean.sdfs, type="n", ylim=c(-28, 0),
     xlab="Frequency", ylab="Posterior SDF (dB)")

plot.CI(fs,
        lower=simband.post.sdfs$a,
        upper=simband.post.sdfs$b,
        col="lightgray")

lines(fs, post.mean.sdfs)

mtext("(b)", side=3, line=0, cex=0.7)

dev.off()


