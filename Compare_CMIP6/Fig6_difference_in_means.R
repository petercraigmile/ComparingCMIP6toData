

library(parallel)
library(splines)
library(scales)
library(excursions)

source("../functions/R_obs_model_post_summaries.R")
source("../functions/trend_ARp_modeling.R")
source("../functions/plot_CI.R")
source("../functions/AR.R")
source("../functions/avg_diff.R")

load("../Derived_Data/CMIP6_tas_historical.RData")
load("../Derived_Data/data_products.RData")
load("../Derived_Data/posterior_summaries_AR4_basis8.RData")

source("../functions/fit_CMIP6_models.R")
source("../functions/colors.R")


hh <- which(data.prods$labels=="HadCRUT5"); hh

avg.diff(data.prods$global.anoms[,hh],
         data.prods$years)



CMIP6.avg.diff <- sapply(1:length(CMIP6$runs), function (k)
                         avg.diff(CMIP6$model.anoms[,k], CMIP6$years))


load("../R_observational_model/chains/data_products_corr_AR4_basis8.RData")

Y.post <- simplify2array(dch$Y.chain)

post.avg.diffs <- sapply(1:ncol(Y.post), function (j)
    avg.diff(Y.post[,j], year=dch$years))

lowerCI <- quantile(post.avg.diffs, 0.025)
upperCI <- quantile(post.avg.diffs, 0.975)

print(round(mean(post.avg.diffs), 3))
print(round(lowerCI, 3))
print(round(upperCI, 3))

##1995-2014 minus 1880-1899

num.models <- max(CMIP6$nums)

pdf(file="../figures/Fig6_difference_in_means.pdf", width=5.4, height=7.5)
par(mfrow=c(1,1), cex=0.55, mar=c(3,9.7,0.5,0.5), mgp=c(1.8,0.5,0), bty="L")

plot(CMIP6.avg.diff, num.models-CMIP6$num+1, yaxt="n", ylab="", 
     type="n", ylim=c(1+1, num.models-1), xlim=c(0, 1.55),
     xlab=expression(paste("Difference in means (",degree,"C)")))

mtext(unique(CMIP6$model.names), at=num.models:1, side=2, las=1, cex=0.55, line=0.2,
      col=the.cols[1:num.models])

rect(lowerCI, -10,
     upperCI, 70, col="gray", border=NA)

points(CMIP6.avg.diff, num.models-CMIP6$num+1, 
       pch=20, cex=1.2,
       xlab="", ylab="", col=the.cols[CMIP6$num])

dev.off()





## Calculating number inside the band for the data,
## number below the band, and number above the band.

are.inside <- lowerCI <= CMIP6.avg.diff & CMIP6.avg.diff <= upperCI

are.lower <- CMIP6.avg.diff < lowerCI

are.higher <- CMIP6.avg.diff > upperCI

n.inside <- sum(are.inside)

n.lower <- sum(are.lower)

n.higher <- sum(are.higher)

ns <- c(n.inside, n.lower, n.higher)

print(ns)
## [1]  14 197 107


pct.lower  <- sapply(tapply(are.lower, CMIP6$model.names, function (x) x), mean)
pct.upper  <- sapply(tapply(are.higher, CMIP6$model.names, function (x) x), mean)
pct.inside <- sapply(tapply(are.inside, CMIP6$model.names, function (x) x), mean)

pct.lower + pct.upper + pct.inside

every5.latex <- function (aa) {
    
    for (j in 1:length(aa)) {
        
        cat(aa[j], " ")
        if (j%%5==0) cat(" \\\\\n")
    }
}


every5.latex( unames[pct.lower==1] )

every5.latex( unames[pct.inside==1] )

every5.latex( unames[pct.upper==1] )

length(unames[pct.lower==1])
length(unames[pct.upper==1])



unames <- unique(CMIP6$model.names)

## cover

cover <- unames[sapply(sel.models, function (x)
    any(CMIP6.avg.diff[x] <= lowerCI) & 
    any(CMIP6.avg.diff[x] >= upperCI))]


every5.latex( cover )



