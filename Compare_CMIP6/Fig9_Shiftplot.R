
load("../Derived_Data/CMIP6_tas_historical.RData")
load("../Derived_Data/data_products.RData")
source("../functions/trend_ARp_modeling.R")
source('../functions/shiftplot.R')

library(parallel)
library(splines)
library(forecast)


num.models <- max(CMIP6$num)


X <- trend.design.matrix(CMIP6$years)

#fit and remove trends
trend.AR4.models <- mclapply(1:ncol(CMIP6$model.anoms), function (k) {
    if (k%%50==0) cat(k, " ")
    fit.trend.ARMA.model(CMIP6$model.anoms[,k], X, 4) }, mc.cores=2)

res.mod=NULL
 trend=list(NULL)   
for (i in 1:ncol(CMIP6$model.anoms)) {
	res.mod = c(res.mod,trend.AR4.models[[i]]$residuals)
	trend[[i]]=trend.AR4.models[[i]]$est.trend
	}


data=data.prods$global.anoms

trend.data=mclapply(1:ncol(data), function (k) {
	fit.trend.ARMA.model(data[,k], X, 4) }, mc.cores=2)

res.dat=NULL
for (i in 1:ncol(data)) {res.dat = c(res.dat,trend.data[[i]]$residuals)}

par(mar=c(6,6,2,1)+.1)

plot(range(CMIP6$years),range(c(res.mod,res.dat)),type="n",xlab="Year",ylab="Residual from trend")
for(i in 1:ncol(CMIP6$model.anoms)) lines(CMIP6$years,trend.AR4.models[[i]]$residuals,lwd=0.5)
for(i in 1:ncol(data)) lines(CMIP6$years,trend.data[[i]]$residuals,col="blue")





par(mfrow=c(1,2))
hist(res.dat,main="Observations",freq=F,xlab="Residuals from trend")
hist(res.mod,main="Models",freq=F,xlab="Residuals from trend")



pdf(file="../figures/Fig9_shiftplot.pdf", width=5, height=4.5)
par(mfrow=c(1,1), cex=0.6, mar=c(3,3,1,0.5), mgp=c(1.8,0.5,0), bty="L")

#shiftplot of residuals
shiftplot(res.dat,res.mod,
          xlab=expression(paste("Observations (",degree,"C)" )),
          ylab=expression(paste("Shift (",degree,"C)" )))

dev.off()


if (FALSE) {

#plot trends
pdf(file="figures/CMIP6_trends_by_model.pdf", width=6.3, height=6)
par(mfrow=c(5,4), cex=0.6, mar=c(2.2,2.2,1,0.5), mgp=c(1.8,0.5,0), bty="L")
for (k in 1:num.models) {

    sel <- CMIP6$num==k
	 trend.ests=NULL  
    for(j in (1:ncol(CMIP6$model.anoms))[sel]) {trend.ests <- rbind(trend.ests,trend[[j]])}  
      
    model.name <- unique(CMIP6$model.names)[k]
 
    matplot(CMIP6$years, t(trend.ests), type="l", xlab="", ylab="", lty=1, col=k,
            ylim=c(-0.5,2))
    mtext(model.name, side=3, line=0, cex=0.7)
}
dev.off()
 
 #plot residuals
 pdf(file="figures/CMIP6_trend_residuals_by_model.pdf", width=6.3, height=6)
par(mfrow=c(5,4), cex=0.6, mar=c(2.2,2.2,1,0.5), mgp=c(1.8,0.5,0), bty="L")
for (k in 1:num.models) {

    sel <- CMIP6$num==k
    
	 res=NULL  
	 
    for(j in (1:ncol(CMIP6$model.anoms))[sel]) 
    {res <- rbind(res,trend.AR4.models[[j]]$residuals)}  

      
    model.name <- unique(CMIP6$model.names)[k]
 
    matplot(CMIP6$years, as.matrix(t(res)), type="l", xlab="", ylab="", lty=1, col=k,
            ylim=c(-0.6,0.5))
    mtext(model.name, side=3, line=0, cex=0.7)
}
dev.off()
}
