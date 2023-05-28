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



label <- function (k, num) {

    text(-0.02, -1, group[k], pos=4, cex=1.25, col=the.cols[k])
    text(-0.02, -5, stub[k],  pos=4, cex=1.05, col=the.cols[k])

    if (!missing(num)) {

        text(0.52, -1, paste(num),
             pos=2, cex=1.25, col="black")
    }
}

the.ylim <- c(-35, 0)

the.ylab <- "SDF (dB)"
the.xlab <- "Frequency (1/year)"

num.runs <- table(mnames)



pdf(file="../figures/FigS_SDF_averaging.pdf", width=5.4, height=5.5)
par(mfrow=c(4,4), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(the.subset)) {

    k <- the.subset[j]

    ss <- avg.sdf.ests[,k]
    
    matplot(fs, ss, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=the.ylim)

    post.sdf.CI.plot()

    lines(fs, ss, lty=1, col=alpha(the.cols[k], the.alpha))

    label(k, num.runs[k])
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


