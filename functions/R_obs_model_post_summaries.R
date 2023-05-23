

summarize.Y <- function (model) {

    Y <- simplify2array(model$Y.chain)
    bb <- model$X %*% simplify2array(model$beta.chain)
    
    exc.Y <- simconf.mc(Y, 0.05)

    post.mean.Y <- rowMeans(Y)

    post.sd.Y <- apply(Y, 1, sd)

    list(years = model$years,
         post.mean.Y = post.mean.Y,
         post.sd.Y   = post.sd.Y,
         exc.Y       = exc.Y)
}


post.Y.plot <- function (Y.summ, par=TRUE) {

    if (par) {
        par(mfrow=c(1,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 
    }
    
    plot(Y.summ$years, Y.summ$post.mean.Y,
         type="n", ylim=cylim, xlim=c(1880, 2020), xlab="Year", ylab=anomaly.label)
    
    plot.CI(Y.summ$years, 
            lower=Y.summ$exc.Y$a, upper=Y.summ$exc.Y$b,
            col="lightgray")
    
    mtext("(a)", side=3, line=0, cex=0.7)
    
    lines(Y.summ$years, Y.summ$post.mean.Y)
    
    plot(Y.summ$years, Y.summ$post.sd.Y, type="l", ylim=c(0.015,0.04),
         xlim=c(1880, 2020),
         xlab="Year", ylab="Posterior SD")
    
    mtext("(b)", side=3, line=0, cex=0.7)    
}




summarize.trend.sdfs <- function (model) {

    fs <- seq(0, 1/2, length=256)
    
    ##YY <- simplify2array(model$Y.chain)
    trends <- model$X %*% simplify2array(model$beta.chain)
    
    sdfs <- sapply(1:length(model$eta.chain), function (k) {
        dB(ar.sdf(fs, AR.pacf.to.ar(to.pacf(model$eta.chain[[k]])),
                  model$sigma2.chain[[k]])) })
    
    exc.trends   <- simconf.mc(trends,   0.05)
    
    exc.sdfs <- simconf.mc(sdfs, 0.05)

    list(years=model$years,
         trends=trends,
         fs=fs,
         post.trend=rowMeans(trends),
         sdfs=sdfs,
         post.sdf=rowMeans(sdfs),
         exc.trends=exc.trends,
         exc.sdfs=exc.sdfs)
}




summarize.trend.sdfs <- function (model) {

    fs <- seq(0, 1/2, length=256)
    
    ##YY <- simplify2array(model$Y.chain)
    trends <- model$X %*% simplify2array(model$beta.chain)
    
    sdfs <- sapply(1:length(model$eta.chain), function (k) {
        dB(ar.sdf(fs, AR.pacf.to.ar(to.pacf(model$eta.chain[[k]])),
                  model$sigma2.chain[[k]])) })
    
    exc.trends   <- simconf.mc(trends,   0.05)
    
    exc.sdfs <- simconf.mc(sdfs, 0.05)

    list(years=model$years,
         trends=trends,
         fs=fs,
         post.trend=rowMeans(trends),
         sdfs=sdfs,
         post.sdf=rowMeans(sdfs),
         exc.trends=exc.trends,
         exc.sdfs=exc.sdfs)
}



post.trends.sdfs <- function (trends.sdfs.summ) {

    par(mfrow=c(1,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 

    plot(trends.sdfs.summ$years, trends.sdfs.summ$post.trend, type="n", ylim=cylim,
         xlab="Year", ylab=expression(paste("Posterior Trend (",degree,"C)")))
    
    plot.CI(trends.sdfs.summ$years, 
            lower=trends.sdfs.summ$exc.bb$a, upper=trends.sdfs.summ$exc.bb$b,
            col="lightgray")
    
    lines(trends.sdfs.summ$years, trends.sdfs.summ$post.trend)
    
    mtext("(a)", side=3, line=0, cex=0.7)   
    
    plot(trends.sdfs.summ$fs, trends.sdfs.summ$post.sdf, type="n", ylim=c(-28, 0),
         xlab="Frequency", ylab="Posterior SDF (dB)")
    
    plot.CI(trends.sdfs.summ$fs, 
            lower=trends.sdfs.summ$exc.sdfs$a, upper=trends.sdfs.summ$exc.sdfs$b,
            col="lightgray")
    
    lines(trends.sdfs.summ$fs, trends.sdfs.summ$post.sdf)
    
    mtext("(b)", side=3, line=0, cex=0.7)
}




plot.discrepancy <- function (model) {

    for (j in 1:model$J) {
        
        delta.j <- sapply(model$delta.chain, function (x) x[,j])
        
        exc.delta.j <- simconf.mc(delta.j, 0.05)
        
        plot(model$years, rowMeans(delta.j), type="n", ylim=c(-0.15,0.15),
             xlab="Year", ylab=expression(paste("Discrepancy (",degree,"C)")),
             yaxt="n", xlim=c(1880-2, 2020+2))
        
        axis(side=2, at=c(-0.10, 0, 0.10))
        
        mtext(data.prods$labels[j], line=0, side=3, cex=0.75)
        
        abline(h=0, lty=1)
        
        plot.CI(model$years, delta.j,
                exc.delta.j$a, exc.delta.j$b,
                col="lightgray")
        
        lines(model$years, rowMeans(delta.j))
    }
}



R.post.summary <- function (model) {
    
    R.mean <- R.025 <- R.975 <- matrix(NA, model$J, model$J)
    
    for (j in 1:model$J) {
        for (k in 1:model$J) {
            
            rr <- sapply(model$R.chain, function (x) x[j,k])
            
            R.mean[j,k] <- mean(rr)       
            R.025[j,k]  <- quantile(rr, p=0.025)        
            R.975[j,k]  <- quantile(rr, p=0.975)
        }
    }
    
    list(R.mean=R.mean,
         R.025=R.025,
         R.975=R.975)
}





post.trend.CI.plot <- function () {

    plot.CI(CMIP6$year,
            lower=TS$exc.trends$a,
            upper=TS$exc.trends$b,
            col="lightgray")    
}



post.sdf.CI.plot <- function () {
    
    plot.CI(fs,
            lower=TS$exc.sdfs$a,
            upper=TS$exc.sdfs$b,
            col="lightgray")
}
