
library(parallel)
library(splines)
library(scales)

source("../functions/trend_ARp_modeling.R")
source("../functions/plot_CI.R")
source("../functions/figure_utils.R")
source("../functions/AR.R")
source("../functions/R_obs_model_post_summaries.R")

load("../Derived_Data/CMIP6_tas_historical.RData")
load("../Derived_Data/data_products.RData")
load("../Derived_Data/posterior_summaries_AR4_basis8.RData")

source("../functions/fit_CMIP6_models.R")

the.alpha <- 1




m.subset <- c("ACCESS-CM2",
              "AWI-CM-1-1-MR",
               "BCC-CSM2-MR",
               "CanESM5-CanOE",
               "CESM2-FV2",
               "E3SM-1-1-ECA",
              "E3SM-1-1",
              "ECEarth",
               "FGOALS-g3",
               "GISS-E2-2-H",
               "HadGEM3-GC31-LL",
               "INM-CM5-0",
               "MIROC6",
               "MPI-ESM1-2-HR",
               "NorCPM1",
               "UKESM1-0-LL")

the.subset <- match(m.subset, umodels)


label <- function (k, nruns) {

    text(1872, 1.4, group[k], pos=4, cex=1.3, col=the.cols[k])
    text(1872, 1.15, stub[k],  pos=4, cex=1.1, col=the.cols[k])

    if (!missing(nruns)) {

        text(2028, 1.4, paste(nruns),
             pos=2, cex=1.3, col="black")
    }
}


num.runs <- table(mnames)

the.ylab <- expression(paste("Trend (",degree,"C)" ))
the.xlab <- "Year"



pdf(file="../figures/FigS_trend_averaging.pdf", width=5.4, height=5.5)
par(mfrow=c(4,4), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(the.subset)) {

    k <- the.subset[j]

    plot(CMIP6$years, avgs.trend.ests[,k], type="n", xlab="", ylab="", lty=1, col=the.cols[k],
        ylim=c(-0.7,1.5), xlim=c(1880, 2020))

    label(k, num.runs[k])
    
    post.trend.CI.plot()

    lines(CMIP6$years, avgs.trend.ests[,k], lty=1, col=alpha(the.cols[k], the.alpha))
}

for (k in 0:3) {
    mtext(the.ylab,
          at=0.12+0.25*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:3) {
    mtext(the.xlab,
          at=0.12+0.25*k, side=1, outer=TRUE, line=0, cex=0.6)
}
    

dev.off()


