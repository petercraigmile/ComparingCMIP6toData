
library(parallel)
library(splines)

source("functions/trend_ARp_modeling.R")
source("functions/plot_CI.R")
source("functions/avg_diff.R")
source("functions/AR_spectra.R")

load("Derived_Data/CMIP6_tas_historical.RData")
load("Derived_Data/data_products_no_JMA.RData")
load("Derived_Data/simband_post_sdfs.RData")
load("Derived_Data/simband_post_trend.RData")
load("Derived_Data/post_avg_diffs.RData")

source("fit_CMIP6_models.R")



CMIP6.avg.diff <- sapply(1:num.runs, function (k)
                         avg.diff(CMIP6$model.anoms[,k], CMIP6$years))


lowerCI <- quantile(post.avg.diffs, 0.025)
upperCI <- quantile(post.avg.diffs, 0.975)

print(round(mean(post.avg.diffs),3))
print(round(lowerCI,3))
print(round(upperCI,3))



pdf(file="figures/Fig4_difference_in_means.pdf", width=4.05, height=4.5)
par(mfrow=c(1,1), cex=0.55, mar=c(3,8,0.5,0.5), mgp=c(1.8,0.5,0), bty="L")

plot(CMIP6.avg.diff, num.models-CMIP6$num+1, yaxt="n", ylab="", 
     type="n", ylim=c(1+1, num.models-1), xlim=c(0, 1.55),
     xlab=expression(paste("Difference in means, 1995-2014 minus 1880-1899 (",degree,"C)")))

mtext(umodels, at=num.models:1, side=2, las=1, cex=0.4, line=0.2,
      col=the.cols[1:num.models])

rect(lowerCI, -10,
     upperCI, 70, col="gray", border=NA)

points(CMIP6.avg.diff, num.models-CMIP6$num+1, 
       pch=20,
       xlab="", ylab="", col=the.cols[CMIP6$num])

dev.off()





## Calculating number inside the band for the data,
## number below the band, and number above the band.

n.inside <- sum(lowerCI <= CMIP6.avg.diff & CMIP6.avg.diff <= upperCI)

n.lower <- sum(CMIP6.avg.diff < lowerCI)

n.upper <- sum(CMIP6.avg.diff > upperCI)

ns <- c(n.inside, n.lower, n.upper)

print(ns)



