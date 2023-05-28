
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

source("../functions/pfcBayes.R")
source("../functions/AR.R")
source("../functions/cov_Bayes_util.R")

load("../Derived_Data/data_products.RData")

source("obs_updates_corr.R")

dch <- init.model(data.prods$years,
                  data.prods$global.anoms[,1:5],
                  data.prods$global.vars[,1:5],
                  X = cbind(1, bs(data.prods$years, 8)),
                  eta0  = rep(0.4, 2),
                  impute.JMA=0)

model.name <- "AR2_basis8"

source("fit_data_product_model.R")

