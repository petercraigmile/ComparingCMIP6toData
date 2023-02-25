
build.R.indexes <- function (K) {

    R.indexes <- list()
    
    r <- 1
    
    for (j in 1:(K-1)) {
        for (k in (j+1):K) {
            
            R.indexes[[r]] <- c(j,k)
            r <- r + 1
        }
    }

    R.indexes
}


calc.det.interval <- function (R, j, k) {

    R.1  <- R.m1 <- R.0 <- R
    
    R.1[k,j] <- R.1[j,k] <- 1
    R.0[k,j] <- R.0[j,k] <- 0
    R.m1[k,j] <- R.m1[j,k] <- -1
    
    f.1  <- det(R.1)
    f.0  <- det(R.0)
    f.m1 <- det(R.m1)
    
    Pa <- (f.1 + f.m1 - 2*f.0)/2
    Pb <- (f.1 - f.m1)/2
    Pc <- f.0
    
    sort(Re(polyroot(c(Pc, Pb, Pa))))
}


LKJ.lpdf <- function (R, nu) {

    (nu-1) * determinant(R)$modulus
}



BMM.lpdf <- function (R, nu) {

    -0.5 * (nu+nrow(R)+1) * determinant(R)$modulus - 0.5 * nu * sum(log(diag(solve(R))))
}


## update.R <- function (chains, save) {

##     scaled.delta <- t(t(chains$delta) / sqrt(chains$tau2))
    
##     curr.R <- chains$R

##     ll.old1 <- LKJ.lpdf(curr.R, chains$nu) + sum(dmvnorm(scaled.delta, sigma=curr.R, log=TRUE))
    
##     the.jumps <- rep(NA, length(R.indexes))
    
##     for (l in 1:length(R.indexes)) {        

##         j <- R.indexes[[l]][1]
##         k <- R.indexes[[l]][2]
        
##         old.r <- curr.R[j,k]

##         prop.sd <- (1 - old.r^2) / 10
        
##         r.range <- calc.det.interval(curr.R, j, k)

##         new.r <- rnorm.truncated(1, old.r, prop.sd, r.range[1], r.range[2])
        
##         new.R <- curr.R
##         new.R[j,k] <- new.R[k,j] <- new.r

##         ll.new1 <- LKJ.lpdf(new.R, chains$nu) + sum(dmvnorm(scaled.delta, sigma=new.R, log=TRUE)) 

##         ll.new <- ll.new1 - dnorm.truncated(new.r, old.r, prop.sd, r.range[1], r.range[2], log=TRUE)
        
##         ll.old <- ll.old1 - dnorm.truncated(old.r, new.r, prop.sd, r.range[1], r.range[2], log=TRUE)
        
##         jump <- MH(ll.new, ll.old)
        
##         if (jump) {
            
##             curr.R[k,j] <- curr.R[j,k] <- new.r
##             ll.old1 <- ll.new1
##         }

##         the.jumps[l] <- jump
##     }
    
##     chain.update("R", curr.R, save, chains)
##     chain.update("R.jumps", the.jumps, save, chains)
## }



#mean.R.jumps <- function (chains) {
#
#    sapply(1:length(R.indexes), function (j)
#        mean(get.chain("R.jumps", j, env=chains)))
#}


## update.tau2 <- function (chains, save) {

##     K <- length(chains$tau2)
    
##     new.log.tau2 <- rnorm(K, log(chains$tau2), 0.3/K)
##     new.tau2     <- exp(new.log.tau2)

##     ll.new <- sum(dinvgamma(new.tau2,    chains$tau2.s, chains$tau2.r, log=TRUE)) +
##         sum(new.log.tau2)
##     ll.old <- sum(dinvgamma(chains$tau2, chains$tau2.s, chains$tau2.r, log=TRUE)) +
##         sum(log(chains$tau2))

##     SS.new <- sqrt(new.tau2)    %*% t(sqrt(new.tau2))
##     SS.old <- sqrt(chains$tau2) %*% t(sqrt(chains$tau2))
        
##     new.Omega <- SS.new * chains$R
##     Omega     <- SS.old * chains$R
    
##     ll.new <- ll.new + sum(dmvnorm(chains$delta, sigma=new.Omega, log=TRUE))
##     ll.old <- ll.old + sum(dmvnorm(chains$delta, sigma=Omega, log=TRUE))

##     jump <- MH(ll.new, ll.old)
##     chain.update("tau2.jumps", jump, save, chains)

##     if (jump) {
        
##         chain.update("tau2", new.tau2, save, chains)
        
##     } else {

##         chain.update("tau2", chains$tau2, save, chains)
##     }

## }
