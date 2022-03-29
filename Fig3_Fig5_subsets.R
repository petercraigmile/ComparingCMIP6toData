

library(parallel)
library(splines)
library(scales)

source("functions/trend_ARp_modeling.R")
source("functions/plot_CI.R")
source("functions/AR_spectra.R")

load("Derived_Data/CMIP6_tas_historical.RData")
load("Derived_Data/data_products_no_JMA.RData")
load("Derived_Data/simband_post_trend.RData")
load("Derived_Data/simband_post_sdfs.RData")

source("fit_CMIP6_models.R")



m.subset <- c("ACCESS-CM2",
              "BCC-CSM2-MR",
              "CanESM5-CanOE",
              "CESM2-FV2",
              "CMCC-CM2-HR4",
              "E3SM-1-1-ECA",
              "E3SM-1-1",
              "FGOALS-g3",
              "GISS-E2-2-H",
              "HadGEM3-GC31-LL",
              "IITM",
              "INM-CM4-8",
              "MIROC6",
              "MPI-ESM1-2-HR",
              "NorCPM1",
              "UKESM1-0-LL")

the.subset <- match(m.subset, umodels)

the.alpha <- 1


pdf(file="figures/Fig3_CMIP6_trends_subset.pdf", width=4.05, height=5)
par(mfrow=c(4,4), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.5,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in the.subset) {

    tes <- trend.ests[,sel.models[[k]]]
    
    matplot(CMIP6$years, tes, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=c(-0.7,1.5))

    text(1872, 1.35, group[k], pos=4, cex=1.2, col=the.cols[k])
    text(1872, 1.05, stub[k], pos=4, cex=1.05, col=the.cols[k])

    plot.CI(CMIP6$years,
            lower=simband.post.trend$a,
            upper=simband.post.trend$b,
            col="lightgray")

    matlines(CMIP6$years, tes, lty=1, col=alpha(the.cols[k], the.alpha))
}

for (k in 0:3) {
    mtext(expression(paste("Trend (",degree,"C)" )),
          at=0.13+0.25*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:3) {
    mtext("Year",
          at=0.13+0.25*k, side=1, outer=TRUE, line=0, cex=0.6)
}

dev.off()



## ======================================================================

pdf(file="figures/FigS3_CMIP6_trends.pdf", width=4.05, height=6.3)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:30) {

    tes <- trend.ests[,sel.models[[k]]]
    
    matplot(CMIP6$years, tes, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=c(-0.7,1.5))

    text(1872, 1.35, group[k], pos=4, cex=1.2, col=the.cols[k])
    text(1872, 1.05, stub[k], pos=4, cex=0.9, col=the.cols[k])

    plot.CI(CMIP6$years,
            lower=simband.post.trend$a,
            upper=simband.post.trend$b,
            col="lightgray")

    matlines(CMIP6$years, tes, lty=1, col=alpha(the.cols[k], the.alpha))
}

for (k in 0:5) {
    mtext(expression(paste("Trend (",degree,"C)" )),
          at=0.09+0.167*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:5) {
    mtext("Year",
          at=0.10+0.20*k, side=1, outer=TRUE, line=0, cex=0.6)
}

dev.off()

pdf(file="figures/FigS4_CMIP6_trends.pdf", width=4.05, height=6.3)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 31:num.models) {

    tes <- trend.ests[,sel.models[[k]]]
    
    matplot(CMIP6$years, tes, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=c(-0.7,1.5))

    text(1872, 1.35, group[k], pos=4, cex=1.2, col=the.cols[k])
    text(1872, 1.05, stub[k], pos=4, cex=0.9, col=the.cols[k])

    plot.CI(CMIP6$years,
            lower=simband.post.trend$a,
            upper=simband.post.trend$b,
            col="lightgray")

    matlines(CMIP6$years, tes, lty=1, col=alpha(the.cols[k], the.alpha))
}

for (k in 0:5) {
    mtext(expression(paste("Trend (",degree,"C)" )),
          at=0.09+0.167*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:2) {
    mtext("Year",
          at=0.10+0.20*k, side=1, outer=TRUE, line=0, cex=0.6)
}

for (k in 3:4) {
    mtext("Year",
          at=0.10+0.20*k, side=1, outer=TRUE, line=-10.4, cex=0.6)
}

dev.off()




## ======================================================================


label <- function (k) {

    text(0, -2, group[k], pos=4, cex=1.2, col=the.cols[k])
    text(0, -6, stub[k], pos=4, cex=1.05, col=the.cols[k])
}

the.ylim <- c(-30, 0)


pdf(file="figures/Fig5_CMIP6_SDF_subset.pdf", width=4.05, height=5)
par(mfrow=c(4,4), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.5,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in the.subset) {

    ss <- sdf.ests[,sel.models[[k]]]
    
    matplot(fs, ss, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=the.ylim)

    plot.CI(fs,
            lower=simband.post.sdfs$a,
            upper=simband.post.sdfs$b,
            col="lightgray")

    matlines(fs, ss, lty=1, col=alpha(the.cols[k], the.alpha))

    label(k)
}


for (k in 0:3) {
    mtext("SDF (dB)",
          at=0.13+0.25*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:3) {
    mtext("Frequency",
          at=0.13+0.25*k, side=1, outer=TRUE, line=0, cex=0.6)
}

dev.off()

label <- function (k) {

    text(0, -2, group[k], pos=4, cex=1.2, col=the.cols[k])
    text(0, -6, stub[k], pos=4, cex=0.9, col=the.cols[k])
}


pdf(file="figures/FigS6_CMIP6_SDF.pdf", width=4.05, height=6.3)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:30) {

    ss <- sdf.ests[,sel.models[[k]]]
    
    matplot(fs, ss, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=the.ylim)

    plot.CI(fs,
            lower=simband.post.sdfs$a,
            upper=simband.post.sdfs$b,
            col="lightgray")

    matlines(fs, ss, lty=1, col=alpha(the.cols[k], the.alpha))

    label(k)
}

for (k in 0:5) {
    mtext("SDF (dB)",
          at=0.09+0.167*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:5) {
    mtext("Frequency",
          at=0.10+0.20*k, side=1, outer=TRUE, line=0, cex=0.6)
}

dev.off()




pdf(file="figures/FigS7_CMIP6_SDF.pdf", width=4.05, height=6.3)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 31:num.models) {

    ss <- sdf.ests[,sel.models[[k]]]

    matplot(fs, ss, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
                ylim=the.ylim)

    plot.CI(fs,
            lower=simband.post.sdfs$a,
            upper=simband.post.sdfs$b,
            col="lightgray")

    matlines(fs, ss, lty=1, col=alpha(the.cols[k], the.alpha))
    
    label(k)
}

for (k in 0:5) {
    mtext("SDF (dB)",
          at=0.09+0.167*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:2) {
    mtext("Frequency",
          at=0.10+0.20*k, side=1, outer=TRUE, line=0, cex=0.6)
}

for (k in 3:4) {
    mtext("Frequency",
          at=0.10+0.20*k, side=1, outer=TRUE, line=-10.4, cex=0.6)
}

dev.off()



## ======================================================================
## Inside/outside for trend
## ======================================================================

delta <- table(mnames) * .05 

num <- sapply(sel.models, function (x) sum(!trend.inside[x]))
pct <- sapply(sel.models, function (x) mean(!trend.inside[x]))*100
ms <- length(umodels):1

##xcbind(umodels, num, table(names))

pdf(file="figures/FigS5_pct_trends_outside.pdf", width=4.05, height=6)
par(mfrow=c(1,1), cex=0.55, mar=c(3,9.5,0.5,0.5), mgp=c(1.8,0.5,0), bty="L")

plot(pct, ms, yaxt="n", ylab="", 
     type="n", ylim=c(1+1, num.models-1), xlim=c(0, 100),
     xlab="% runs outside simultaneous band for data trend")

mtext(umodels, at=num.models:1, side=2, las=1, cex=0.6, line=0.2,
      col=the.cols[1:num.models])

#rect(lowerCI, -10,
#     upperCI, 70, col="gray", border=NA)

points(pct, ms, 
       pch=ifelse(pct > delta, 16, 15), cex=1.3,
       xlab="", ylab="", col=the.cols)

dev.off()


## ======================================================================
## Inside/outside for SDFs
## ======================================================================


delta <- table(mnames) * .05 

num <- sapply(sel.models, function (x) sum(!sdfs.inside[x]))
pct <- sapply(sel.models, function (x) mean(!sdfs.inside[x]))*100
ms <- length(umodels):1

##cbind(umodels, num, table(mnames))

pdf(file="figures/FigS8_pct_sdfs_outside.pdf", width=4.05, height=6)
par(mfrow=c(1,1), cex=0.55, mar=c(3,9.5,0.5,0.5), mgp=c(1.8,0.5,0), bty="L")

plot(pct, ms, yaxt="n", ylab="", 
     type="n", ylim=c(1+1, num.models-1), xlim=c(0, 100),
     xlab="% runs outside simultaneous band for data SDF")

mtext(umodels, at=num.models:1, side=2, las=1, cex=0.6, line=0.2,
      col=the.cols[1:num.models])

#rect(lowerCI, -10,
#     upperCI, 70, col="gray", border=NA)

points(pct, ms, 
       pch=ifelse(pct > delta, 16, 15), cex=1.3,
       xlab="", ylab="", col=the.cols)

dev.off()


