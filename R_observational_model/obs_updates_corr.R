

init.model <- function (years,
                        global.anoms, global.vars,
                        X, impute.JMA=5,
                        eta0=c(0.4, 0.4)) {
    
    chains <- new.env()

    chains$N <- nrow(global.anoms)
    chains$J <- ncol(global.anoms)

    zz <- rowMeans(global.anoms, na.rm=TRUE)
    sel <- !is.na(zz)
    
    beta0 <- lm.fit(X[sel,], zz[sel])$coef
    
    chains$beta.mu   <- rep(0, length(beta0))
    chains$beta.prec <- diag(rep(1/10, length(beta0)))
    
    chains$sigma2.s <- 0.01
    chains$sigma2.r <- 0.01
    
    chains$eta.mu <- 0.0
    chains$eta.sd <- 0.7

    chains$tau2.s <- 0.01
    chains$tau2.r <- 0.01

    chains$nu <- 1.1 

    chains$years    <- years
    chains$anoms    <- global.anoms
    chains$dvars    <- global.vars
    chains$X        <- X
    
    chains$impute.JMA <- impute.JMA

    if (impute.JMA>0) {

        chains$anoms[1:11, impute.JMA] <- rnorm(11,
                                                mean(global.anoms, na.rm=TRUE),
                                                sd(global.anoms, na.rm=TRUE))
        
        chains$dvars[1:11, impute.JMA] <- chains$dvars[12, impute.JMA]

        chain("JMA", chains$anoms[1:11, impute.JMA], chains)
    }
    
    chains$R.indexes <- build.R.indexes(chains$J)
    
    R0 <- matrix(0.4, chains$J, chains$J)
    diag(R0) <- 1

    chain("R", R0, chains)
    chain("R.jumps", FALSE, chains)

    chain("tau2", 0.03, chains)
   
    chain("delta",
          matrix(rnorm(chains$N * chains$J, 0, chains$tau2), chains$N, chains$J),
          chains)

    chain("beta", beta0, chains)
    
    chain("eta", eta0, chains)
    chain("sigma2", 0.1, chains)
    chain("eta.sigma2.jumps", FALSE, chains)

    chain("Y", rnorm(chains$N, 0, sqrt(chains$sigma2)), chains)


    chains
}



calc.Sigma <- function (N, eta, sigma2) {

    toeplitz(AR.pacf.acvs(N-1, to.pacf(eta), sigma2))
}






update.tau2 <- function (chains, save) {

    new.tau2 <- 1/rgamma(1,
                         chains$tau2.s + 0.5 * chains$N * chains$J,
                         chains$tau2.r + 0.5 * sum(chains$delta^2))

    chain.update("tau2", new.tau2, save, chains)
}




update.JMA <- function (chains, save) {
    ## Assumes the JMA data product is at index chains$impute.JMA and
    ## the first 11 observations are missing.

    JMA <- chains$impute.JMA
    
    if (JMA > 0) {

        new.JMA <- sapply(1:11, function (tt) {
            
            ss <- diag(sqrt(chains$dvars[tt,]))
            
            cov <- ss %*% chains$R %*% ss
            
            bb <- solve(cov[-JMA, ][, -JMA], cov[JMA, -JMA])
            
            the.sd <- sqrt(cov[JMA,JMA] - sum(cov[JMA, -JMA] * bb))
            
            res <- chains$anoms[tt, -JMA] - chains$Y[tt] - chains$delta[tt, -JMA]
            
            the.mean <- sum(bb * res)
            
            chains$Y[tt] + chains$delta[tt, JMA] + rnorm(1, the.mean, the.sd)
        })
        
        chain.update("JMA", new.JMA, save, chains)
        
        D.impute            <- chains$anoms
        D.impute[1:11, JMA] <- new.JMA
        
        assign("anoms", D.impute, envir=chains)
    }
}
    



update.Y <- function (chains, save) {

    mu        <- drop(chains$X %*% chains$beta)
    Sigma.inv <- solve(calc.Sigma(chains$N, chains$eta, chains$sigma2))

    P <- Sigma.inv
    m <- Sigma.inv %*% mu

    R.inv <- solve(chains$R)
    
    for (t in 1:chains$N) {

        ss <- diag(sqrt(1/chains$dvars[t,]))

        AA <- colSums(ss %*% R.inv %*% ss)

        P[t,t] <- P[t,t] + sum(AA)
        m[t]   <- m[t]   + crossprod(AA, chains$anoms[t,] - chains$delta[t,])
    }
    
    new.Y <- rmvnorm.cond.precision(m, P)
    
    chain.update("Y", new.Y, save, chains)
}






update.delta <- function (chains, save) {

    new.delta <- matrix(NA, chains$N, chains$J)

    tau.inv.mat <- diag(rep(1/chains$tau2, chains$J))

    R.inv <- solve(chains$R)

    for (t in 1:chains$N) {
        
        ss <- diag(sqrt(1/chains$dvars[t,]))
        
        Vt.inv <- ss %*% R.inv %*% ss

        P <- Vt.inv + tau.inv.mat
        
        m <- crossprod(Vt.inv, chains$anoms[t,] - chains$Y[t])
        
        new.delta[t,] <-  rmvnorm.cond.precision(m, P)
    }
    
    chain.update("delta", new.delta, save, chains)
}




update.beta <- function (chains, save) {

    Sigma <- calc.Sigma(length(chains$Y), chains$eta, chains$sigma2)

    B <- solve(Sigma, chains$X)
    P <- crossprod(chains$X, B) + chains$beta.prec
    m <- crossprod(B, chains$Y) + crossprod(chains$beta.prec, chains$beta.mu)

    new.beta <- rmvnorm.cond.precision(m, P)

    chain.update("beta", new.beta, save, chains)
}



update.eta.sigma2 <- function (chains, save) {
    
    new.eta <- rnorm(length(chains$eta), chains$eta, 0.15)
    
    new.log.sigma2 <- rnorm(1, log(chains$sigma2), 0.15)
    new.sigma2     <- exp(new.log.sigma2)

    mu <- drop(chains$X %*% chains$beta)

    new.Sigma <- calc.Sigma(length(chains$Y), new.eta,    new.sigma2)    
    Sigma     <- calc.Sigma(length(chains$Y), chains$eta, chains$sigma2)

    ll.new <- dmvnorm(chains$Y, mu, new.Sigma, log=TRUE) +
                sum(dnorm(new.eta, chains$eta.mu, chains$eta.sd, log=TRUE)) +
                  dinvgamma(new.sigma2, chains$sigma2.s, chains$sigma2.r, log=TRUE) + new.log.sigma2
    
    ll.old <- dmvnorm(chains$Y, mu, Sigma, log=TRUE) +
                sum(dnorm(chains$eta, chains$eta.mu, chains$eta.sd, log=TRUE)) +
                  dinvgamma(chains$sigma2, chains$sigma2.s, chains$sigma2.r, log=TRUE) + log(chains$sigma2)

    jump <- MH(ll.new, ll.old)

    chain.update("eta.sigma2.jumps", jump, save, chains)

    if (jump) {

        chain.update("eta",    new.eta,    save, chains)
        chain.update("sigma2", new.sigma2, save, chains)
        
    } else {

        chain.update("eta",    chains$eta,    save, chains)
        chain.update("sigma2", chains$sigma2, save, chains)
    }
}






update.R <- function (chains, save) {

    zz <- chains$anoms - chains$delta - chains$Y
    for (t in 1:chains$N) {

        zz[t,] <- zz[t,] / sqrt(chains$dvars[t,])
    }
    
    curr.R <- chains$R

    ll.old1 <- LKJ.lpdf(curr.R, chains$nu) + sum(dmvnorm(zz, sigma=curr.R, log=TRUE))
    
    the.jumps <- rep(NA, length(chains$R.indexes))
    
    for (l in sample(1:length(chains$R.indexes))) {

        j <- chains$R.indexes[[l]][1]
        k <- chains$R.indexes[[l]][2]
        
        old.r <- curr.R[j,k]

        prop.sd <- R.prop.sds[l]
        
        r.range <- calc.det.interval(curr.R, j, k)

        new.r <- rnorm.truncated(1, old.r, prop.sd, r.range[1], r.range[2])
        
        new.R <- curr.R
        new.R[j,k] <- new.R[k,j] <- new.r

        ll.new1 <- LKJ.lpdf(new.R, chains$nu) + sum(dmvnorm(zz, sigma=new.R, log=TRUE)) 

        ll.new <- ll.new1 - dnorm.truncated(new.r, old.r, prop.sd, r.range[1], r.range[2], log=TRUE)
        
        ll.old <- ll.old1 - dnorm.truncated(old.r, new.r, prop.sd, r.range[1], r.range[2], log=TRUE)
        
        jump <- MH(ll.new, ll.old)
        
        if (jump) {
            
            curr.R[k,j] <- curr.R[j,k] <- new.r
            ll.old1 <- ll.new1
        }

        the.jumps[l] <- jump
    }
    
    chain.update("R", curr.R, save, chains)
    chain.update("R.jumps", the.jumps, save, chains)
}




trace.plots <- function (chains) {

    print(mean(simplify2array(chains$eta.sigma2.jumps.chain)))
    
    par(mfrow=c(6,4), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

    for (j in 1:length(chains$eta)) {
        
        plot(get.chain("eta", j, env=chains), type="l", ylab=paste("eta", j), xlab="")
    }

    plot(simplify2array(chains$sigma2.chain), type="l", ylab="sigma2", xlab="")
    plot(simplify2array(chains$tau2.chain), type="l", ylab="tau2", xlab="")


    for (j in 1:length(chains$beta)) {
        
        plot(get.chain("beta", j, env=chains), type="l", ylab=paste("beta", j), xlab="")
    }

    for (j in 1:(chains$J-1)) {
        for (k in (j+1):chains$J) {
            plot(sapply(chains$R.chain, function (x) x[j,k]),
                 type="l", ylab=paste("R[", j, ",", k, "]", sep=""), xlab="")
        }
    }

    if (chains$impute.JMA > 0) {

        for (tt in 1:11) {

            plot(sapply(chains$JMA.chain, function (x) x[tt]),
                        type="l", ylab=paste("JMA[", tt, "]", sep=""), xlab="")
        }
    }
}



JMA.post.plot <- function (chains) {

    K <- chains$impute.JMA
    
    jj <- simplify2array(chains$JMA.chain)
    
    plot(chains$years, chains$anoms[,K], type="n", xlab="Year", ylab="JMA",
         ylim=c(-0.8,0.8))
    
    lines(chains$years[-(1:11)], chains$anoms[-(1:11),K])
    
    points(chains$years[1:11], rowMeans(jj), col="blue", pch=20, cex=0.5)
    
    segments(chains$years[1:11], apply(jj, 1, quantile, p=0.025),
             chains$years[1:11], apply(jj, 1, quantile, p=0.975), col="blue", lwd=2)      }
