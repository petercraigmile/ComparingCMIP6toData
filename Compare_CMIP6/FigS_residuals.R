
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





pdf(file="../figures/FigS_CMIP6_residuals.pdf", width=10, height=10)
par(mfrow=c(6,6), cex=0.6, mar=c(2.8,2.8,1.1,0.5), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:num.runs) {

    plot(CMIP6$years, CMIP6$model.anoms[,k] - trend.ests[,k], type="l", xlab="", ylab=CMIP6$runs[k], lty=1, col=the.cols[CMIP6$nums[k]],
         ylim=c(-0.7,0.6), xlim=c(1880, 2020))

    mtext(paste(CMIP6$model.names[k]), side=3, line=0, cex=0.8)
}


dev.off()


ores <- Y.summ$post.mean.Y - TS$post.trend


pdf(file="../figures/FigS_post_mean_obs_residuals.pdf", width=5, height=3.5)
par(mfrow=c(1,1), cex=0.6, mar=c(2.8,2.8,1.1,0.5), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:num.runs) {

    plot(TS$years, ores, type="l", xlab="Year", ylab="Posterior mean latent observed residual",
         ylim=c(-0.7,0.6), xlim=c(1880, 2020))
}


dev.off()

print(round(sd(ores), 2))



plot(TS$years, Y.summ$post.mean.Y - TS$post.trend, type="l")




res <- CMIP6$model.anoms - trend.ests

the.sds <- tapply(apply(res, 2, sd), CMIP6$model.names, function (x) round(x, 2))


sink("SDs.txt")
print(the.sds)

print(round(sort(sapply(the.sds, mean)), 2))
sink()
