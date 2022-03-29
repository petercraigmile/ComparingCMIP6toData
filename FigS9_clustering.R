

library(scales)
library(parallel)
library(splines)

source("functions/trend_ARp_modeling.R")
source("functions/plot_CI.R")
source("functions/AR_spectra.R")

load("Derived_Data/CMIP6_tas_historical.RData")
load("Derived_Data/data_products_no_JMA.RData")
load("Derived_Data/simband_post_trend.RData")
load("Derived_Data/simband_post_sdfs.RData")

##install.packages("dendextend", repos="https://cran.wustl.edu")

library(dendextend)


load("../R_observational_model/chains/data_products_corr_AR4.RData")

post.mean.beta <- rowMeans(simplify2array(dch$beta.chain))

source("fit_CMIP6_models.R")



beta.hats <- sapply(the.models, function (mm) mm$coef[-(1:4)])

## ======================================================================
## Average over runs
## ======================================================================

cc <- unique(CMIP6$cols)

beta.hatsM <- t(sapply(1:nrow(beta.hats), function (j) tapply(beta.hats[j,], CMIP6$num, mean)))
    

Y <- t(cbind(beta.hatsM, post.mean.beta))
dimnames(Y) <- NULL

the.cols2 <- c(the.cols, "black")

dY <- dist(Y)


## Look up members description
cl <- hclust(dY, method="ward.D2")

dend <- as.dendrogram(cl)

ns <- labels(dend)

the.labels <- c(umodels, "DATA")

labels(dend) <- the.labels[ns]
labels_colors(dend) <- the.cols2[ns]



## I rotate manually after creating this figure...

pdf(file="figures/FigS9_trend_clustering_mean.pdf", width=5.5, height=4)
par(mfrow=c(1,1), cex=0.55, mar=c(9,0,0,0), mgp=c(0,0,0), bty="L")

plot(dend, yaxt="n")

dev.off()





pdf(file=paste("figures/clustering.pdf", sep=""),
    width=4.05, height=4)

for (num.clusters in c(2:12)) {

cuts <- cutree(cl, num.clusters)

long.cols <- c(rep(the.cols, table(mnames)), "red")

sel <- sapply(umodels, function (mm) which(mm==mnames)[1])


par(mfrow=c(4,3), cex=0.5, mar=c(1.8,1.8,1,0.4), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:num.clusters) {
    
    ks <- sel[which(cuts==k)]
    
    tes <- trend.ests[,ks]
    
    matplot(CMIP6$years, tes, type="n", xlab="", ylab="", lty=1,
            ylim=c(-0.7,1.5))
    
    mtext(paste("Cluster", k), side=3, line=0, cex=0.6)
    
    plot.CI(CMIP6$year,
            lower=simband.post.trend$a,
            upper=simband.post.trend$b,
            col="lightgray")
    
    matlines(CMIP6$years, tes, col=alpha(long.cols[ks], 1), lty=1)
}

print(sapply(sort(unique(cuts)), function (x) sort(the.labels[cuts==x])))

    
}

dev.off()
