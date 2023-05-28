
source("../functions/plot_CI.R")

load("../Derived_Data/CMIP6_tas_historical.RData")
load("../Derived_Data/data_products.RData")


cylim <- c(-1.1, 1.5)


pdf(file="../figures/Fig1_CMIP6_obs.pdf", width=5.4, height=2.3)
par(mfrow=c(1,1), cex=0.6, mar=c(3,3,1,0.5), mgp=c(1.8,0.5,0), bty="L")

matplot(CMIP6$years, CMIP6$model.anoms, type="l", ylim=cylim, xlim=c(1880, 2020),
       col="gray", lty=1,
       xlab="Year",
       ylab=expression(paste("Global temperature anomaly (",degree,"C)" )))

for (j in 1:4) {
   lines(data.prods$years, data.prods$global.anoms[,j], lwd=2, col="black")
}

dev.off()

