
## ======================================================================
## The data Derived_Data/data_products.RData comes from
##
## https://github.com/petercraigmile/CombinedEstimateGlobalTemperature
##
## We remove JMA from this dataset
## ======================================================================

load("../Derived_Data/data_products.RData")


## Remove JMA

sel <- data.prods$labels != "JMA"

data.prods$global.anoms <- data.prods$global.anoms[,sel]
data.prods$global.vars  <- data.prods$global.vars[,sel]
data.prods$labels       <- data.prods$labels[sel]


## Change anomaly reference period

time.sub <- data.prods$years>=1880 & data.prods$years<=2014

the.means <- colMeans(data.prods$global.anoms[time.sub,])

data.prods$years        <- data.prods$years[time.sub]
data.prods$global.vars  <- data.prods$global.vars[time.sub,]
data.prods$global.anoms <- data.prods$global.anoms[time.sub,]

for (j in 1:4) {

    data.prods$global.anoms[,j] <- data.prods$global.anoms[,j] - the.means[j]
}

save(data.prods, file="../Derived_Data/data_products_no_JMA.RData")


