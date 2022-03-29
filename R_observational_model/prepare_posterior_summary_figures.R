
library(splines)
library(nlme)
library(mvtnorm)
library(excursions)

source("functions/figure_utils.R")
source("../functions/plot_CI.R")
source("functions/AR.R")

load("../Derived_Data/data_products_no_JMA.RData")
load("chains/data_products_corr_AR4.RData")


YY <- simplify2array(dch$Y.chain)
bb <- dch$X %*% simplify2array(dch$beta.chain)

exc.YY <- simconf.mc(YY, 0.05)

pdf(file="../figures/FigS1_BHM_data_products_AR4.pdf", width=4.05, height=3.5)
par(mfrow=c(2,1), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

plot(dch$years, rowMeans(YY),
     type="n", ylim=cylim, xlab="Year", ylab=anomaly.label)

plot.CI(dch$years, YY, 
        exc.YY$a, exc.YY$b,
        col="lightgray")

mtext("(a)", side=3, line=0, cex=0.7)

lines(dch$years, rowMeans(YY))

plot(dch$years, apply(YY, 1, sd), type="l", ylim=c(0.015,0.04),
     xlab="Year", ylab="Posterior SD")

mtext("(b)", side=3, line=0, cex=0.7)

dev.off()





pdf(file="../figures/FigS2_discrepancies_AR4.pdf", width=4.05, height=4.5)
par(mfrow=c(4,1), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

for (j in 1:dch$J) {
    
    delta.j <- sapply(dch$delta.chain, function (x) x[,j])

    exc.delta.j <- simconf.mc(delta.j, 0.05)

    plot(dch$years, rowMeans(delta.j), type="n", ylim=c(-0.15,0.15),
         xlab="Year", ylab=expression(paste("Discrepany (",degree,"C)")),
         yaxt="n", xlim=c(1880-2, 2020+2))

    axis(side=2, at=c(-0.10, 0, 0.10))

    mtext(data.prods$labels[j], line=0, side=3, cex=0.75)

    abline(h=0, lty=1)
    
    plot.CI(dch$years, delta.j,
            exc.delta.j$a, exc.delta.j$b,
            col="lightgray")
    
    lines(dch$years, rowMeans(delta.j))
}

dev.off()


