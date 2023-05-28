


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

source("../functions/fit_CMIP6_models.R")


pdf(file="../figures/FigS_Y_basis_AR.pdf", width=5.4, height=7)
par(mfrow=c(4,2), cex=0.6, mar=c(3,3,1,0.5), oma=c(0,2,0,0), mgp=c(1.8,0.5,0), bty="L")

load("../Derived_Data/posterior_summaries_AR4_basis8.RData")

post.Y.plot(Y.summ, par=FALSE)

load("../Derived_Data/posterior_summaries_AR4_basis6.RData")

post.Y.plot(Y.summ, par=FALSE)

load("../Derived_Data/posterior_summaries_AR2_basis8.RData")

post.Y.plot(Y.summ, par=FALSE)

load("../Derived_Data/posterior_summaries_AR2_basis6.RData")

post.Y.plot(Y.summ, par=FALSE)

mtext("b=8, p=4", at=0.28/2+0.25*0, side=2, outer=TRUE, line=0, cex=0.6)
mtext("b=6, p=4", at=0.28/2+0.25*1, side=2, outer=TRUE, line=0, cex=0.6)
mtext("b=8, p=2", at=0.28/2+0.25*2, side=2, outer=TRUE, line=0, cex=0.6)
mtext("b=6, p=2", at=0.28/2+0.25*3, side=2, outer=TRUE, line=0, cex=0.6)

dev.off()


