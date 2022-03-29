
## ======================================================================
## R code to fit the models used in the article
##
## P. F. Craigmile and P. Guttorp,
## A combined estimate of global temperature
##
## The truncatedNormals R library is available from
## https://github.com/petercraigmile/truncatedNormals
##
## Contact: pfc@stat.osu.edu
## ======================================================================

library(splines)
library(mvtnorm)
library(truncatedNormals)
library(excursions)

source("functions/pfcBayes.R")
source("functions/AR.R")
source("functions/cov_Bayes_util.R")

load("../R/Derived_Data/data_products_no_JMA.RData")


source("obs_updates_corr.R")

dch <- init.model(data.prods$years,
                  data.prods$global.anoms,
                  data.prods$global.vars,
                  X = cbind(1, bs(data.prods$years, 6)),
                  eta0  = rep(0.4, 4),
                  impute.JMA=0)


to.update <- c("Y", "delta", "tau2", "R", "JMA", "beta", "eta.sigma2")


R.prop.sds <- rep(NA, length(dch$R.indexes))

for (k in 1:length(dch$R.indexes)) {

    aa <- dch$R.indexes[[k]]
    R.prop.sds[k] <- 0.02
}

print(system.time(run.MCMC(dch, to.update, 2000, every=500, burn.in=TRUE)))


for (k in 1:50) {
    
    run.MCMC(dch, to.update, 20000, every=2500, thin=20)
    
    cat("R jumps: ")
    print(round(rowMeans(simplify2array(dch$R.jumps.chain)), 2))
    cat("\n")
    
    pdf(file="trace_plots/trace_plots_data_products_AR4.pdf", width=6.3, height=6.5)
    trace.plots(dch)
    dev.off()
    
    save(dch, file="chains/data_products_corr_AR4.RData")
}

