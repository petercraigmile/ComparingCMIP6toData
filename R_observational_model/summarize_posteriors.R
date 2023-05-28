

library(excursions)

load("../R_observational_model/chains/data_products_corr_AR4_basis8.RData")

source("../functions/AR.R")
source("../functions/R_obs_model_post_summaries.R")

TS     <- summarize.trend.sdfs(dch)

Y.summ <- summarize.Y(dch)

Y.post <- simplify2array(dch$Y.chain)

save(list=c("TS", "Y.summ", "Y.post"),
     file="../Derived_Data/posterior_summaries_AR4_basis8.RData")



load("../R_observational_model/chains/data_products_corr_AR2_basis8.RData")

source("../functions/AR.R")
source("../functions/R_obs_model_post_summaries.R")

TS     <- summarize.trend.sdfs(dch)

Y.summ <- summarize.Y(dch)

Y.post <- simplify2array(dch$Y.chain)

save(list=c("TS", "Y.summ", "Y.post"),
     file="../Derived_Data/posterior_summaries_AR2_basis8.RData")





library(excursions)

load("../R_observational_model/chains/data_products_corr_AR4_basis6.RData")

source("../functions/AR.R")
source("../functions/R_obs_model_post_summaries.R")

TS     <- summarize.trend.sdfs(dch)

Y.summ <- summarize.Y(dch)

Y.post <- simplify2array(dch$Y.chain)

save(list=c("TS", "Y.summ", "Y.post"),
     file="../Derived_Data/posterior_summaries_AR4_basis6.RData")



library(excursions)

load("../R_observational_model/chains/data_products_corr_AR2_basis6.RData")

source("../functions/AR.R")
source("../functions/R_obs_model_post_summaries.R")

TS     <- summarize.trend.sdfs(dch)

Y.summ <- summarize.Y(dch)

Y.post <- simplify2array(dch$Y.chain)

save(list=c("TS", "Y.summ", "Y.post"),
     file="../Derived_Data/posterior_summaries_AR2_basis6.RData")

