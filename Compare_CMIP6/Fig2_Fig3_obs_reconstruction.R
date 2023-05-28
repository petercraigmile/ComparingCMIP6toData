


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


pdf(file="../figures/Fig2_Y_summaries.pdf", width=5.4, height=2.3)
par(mfrow=c(1,2), cex=0.6, mar=c(3,3,1,0.5), mgp=c(1.8,0.5,0), bty="L")

post.Y.plot(Y.summ, par=FALSE)

dev.off()



pdf(file="../figures/Fig3_trends_sdf_obs.pdf", width=5.4, height=2.3)
par(mfrow=c(1,2), cex=0.6, mar=c(3,3,1,0.5), mgp=c(1.8,0.5,0), bty="L")

plot(CMIP6$year, TS$post.trend, type="n", ylim=cylim, xlim=c(1880, 2020),
     xlab="Year", ylab=anomaly.label)

post.trend.CI.plot()

lines(CMIP6$year, TS$post.trend)

mtext("(a)", side=3, line=0, cex=0.7)


plot(fs, TS$post.sdf, type="n", ylim=c(-28, 0),
     xlab="Frequency (1/year)", ylab="Posterior SDF (dB)")

post.sdf.CI.plot()

lines(fs, TS$post.sdf)

mtext("(b)", side=3, line=0, cex=0.7)

dev.off()


