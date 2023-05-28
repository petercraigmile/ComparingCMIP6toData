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

the.ylim <- c(-30, 0)



## ======================================================================
## Inside/outside for SDFs
## ======================================================================


#delta <- table(mnames) * .05 

num.outside <- sapply(sel.models, function (x) sum(!sdfs.inside[x]))
num.inside <- sapply(sel.models, function (x) sum(sdfs.inside[x]))
pct.inside <- sapply(sel.models, function (x) mean(sdfs.inside[x]))*100
ms <- length(umodels):1



unames <- unique(CMIP6$model.names)
    
unames[pct.inside == 100]

## "E3SM-1-1-ECA" "E3SM-1-1"     "IITM"

print(sum(num.outside > 0))

sel <- num.outside > 0

unames[sel][pct.inside[sel] == 0]

length(unames[sel][num.inside[sel] == 0])
length(unames[sel][num.inside[sel] > 0])




label <- function (k,pct) {

    text(-0.02, -1, group[k], pos=4, cex=1.25, col=the.cols[k])
    text(-0.02, -5, stub[k],  pos=4, cex=1.05, col=the.cols[k])

    if (!missing(pct)) {

        text(0.52, -1, paste(round(pct, 0), "%", sep=""),
             pos=2, cex=1.25, col="black")
    }
}


the.ylab <- "SDF (dB)"
the.xlab <- "Frequency (1/year)"



pdf(file="../figures/Fig7_CMIP6_SDF1.pdf", width=5.4, height=7.5)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:30) {

    ss <- sdf.ests[,sel.models[[k]]]
    
    matplot(fs, ss, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=the.ylim)

    post.sdf.CI.plot()

    matlines(fs, ss, lty=1, col=alpha(the.cols[k], the.alpha))

    label(k, pct.inside[k])
}

for (k in 0:5) {
    mtext(the.ylab,
          at=0.09+0.167*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:5) {
    mtext(the.xlab,
          at=0.10+0.20*k, side=1, outer=TRUE, line=0, cex=0.6)
}

dev.off()




pdf(file="../figures/Fig8_CMIP6_SDF2.pdf", width=5.4, height=7.5)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 31:num.models) {

    ss <- sdf.ests[,sel.models[[k]]]

    matplot(fs, ss, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
                ylim=the.ylim)

    post.sdf.CI.plot()

    matlines(fs, ss, lty=1, col=alpha(the.cols[k], the.alpha))
    
    label(k, pct.inside[k])
}

for (k in 0:5) {
    mtext(the.ylab,
          at=0.09+0.167*k, side=2, outer=TRUE, line=0, cex=0.6)
}

for (k in 0:2) {
    mtext(the.xlab,
          at=0.10+0.20*k, side=1, outer=TRUE, line=0, cex=0.6)
}

for (k in 3:4) {
    mtext(the.xlab,
          at=0.10+0.20*k, side=1, outer=TRUE, line=-12.4, cex=0.6)
}

dev.off()


sum(num.outside)

sum(num.inside == 0)
sum(num.inside > 0)



dims <- dim(TS$sdfs)

inds <- sort(sample(dims[1], 25))

ls <- sample(dims[2], 1000)

par(mfrow=c(5,5), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

for (j in 1:length(inds)) {

    y <- TS$sdfs[inds[j],ls]

    qqnorm(y, main="", xlab="", ylab="")
    qqline(y)
}
