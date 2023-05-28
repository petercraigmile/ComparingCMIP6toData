
source("../functions/figure_utils.R")
load("../Derived_Data/data_products.RData")

#K <- ncol(data.prods$global.anoms)
K <- 5

pdf(file="figures/Fig1_EDA_data_products.pdf", width=6.4, height=2.2)
par(mfrow=c(1,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

plot(data.prods$years,  data.prods$global.anoms[,1], type="n",
     xlab="Year", ylab=anomaly.label, ylim=cylim)

for (j in 1:K) {
    
    lines(data.prods$years,  data.prods$global.anoms[,j], col=the.cols[j])
}

legend(1880, cylim[2], data.prods$labels[1:K], col=the.cols[1:K], lty=1, cex=1, bty="n")

mtext("(a)", side=3, line=0, cex=0.7)

plot(data.prods$years,  sqrt(data.prods$global.vars[,1]), type="n",
     xlab="Year", ylab=expression(paste("Standard error (",degree,"C)" )), ylim=c(0, 0.12))

for (j in 1:K) {
    
    lines(data.prods$years, sqrt(data.prods$global.vars[,j]), col=the.cols[j])
}

mtext("(b)", side=3, line=0, cex=0.7)

dev.off()
