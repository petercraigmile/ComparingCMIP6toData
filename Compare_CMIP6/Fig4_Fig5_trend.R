
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



## ======================================================================
## Inside/outside for trend
## ======================================================================

delta <- table(mnames) * .05 

num.outside <- sapply(sel.models, function (x) sum(!trend.inside[x]))
num.inside <- sapply(sel.models, function (x) sum(trend.inside[x]))
pct.inside <- sapply(sel.models, function (x) mean(trend.inside[x]))*100

unames <- unique(CMIP6$model.names)
    
unames[pct.inside == 100]

#"CMCC-CM2-HR4"  "CNRM-CM6-1-HR" "CNRM-CM6-1"    "FGOALS-f3-L"  
#"FGOALS-g3"     "GISS-E2-2-H"   "INM-CM4-8"     "MPI-ESM1-2-HR"
#"MPI-ESM1-2-LR" "NorCPM1"      

print(sum(num.outside > 0))

sel <- num.outside > 0

unames[sel][pct.inside[sel] == 0]

length(unames[sel][num.inside[sel] == 0])
length(unames[sel][num.inside[sel] > 0])



label <- function (k, pct) {

    text(1872, 1.4, group[k], pos=4, cex=1.3, col=the.cols[k])
    text(1872, 1.15, stub[k],  pos=4, cex=1.1, col=the.cols[k])

    if (!missing(pct)) {
        text(2028, 1.4, paste(round(pct, 0), "%", sep=""),
             pos=2, cex=1.3, col="black")
    }
}


pdf(file="../figures/Fig4_CMIP6_trends1.pdf", width=5.4, height=7.5)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:30) {

    tes <- trend.ests[,sel.models[[k]]]
    
    matplot(CMIP6$years, tes, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=c(-0.7,1.5), xlim=c(1880, 2020))

    label(k, pct.inside[k])
    
    post.trend.CI.plot()

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




pdf(file="../figures/Fig5_CMIP6_trends2.pdf", width=5.4, height=7.5)
par(mfrow=c(6,5), cex=0.5, oma=c(1.3,1.3,0,0),
    mar=c(1.5,1.5,0.6,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 31:num.models) {

    tes <- trend.ests[,sel.models[[k]]]
    
    matplot(CMIP6$years, tes, type="n", xlab="", ylab="", lty=1, col=the.cols[k],
            ylim=c(-0.7,1.5), xlim=c(1880, 2020))

    label(k, pct.inside[k])

    post.trend.CI.plot()

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
          at=0.10+0.20*k, side=1, outer=TRUE, line=-12.4, cex=0.6)
}

dev.off()


