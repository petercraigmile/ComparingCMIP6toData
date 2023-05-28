

to.update <- c("Y", "delta", "tau2", "R", "JMA", "beta.eta.sigma2")

R.prop.sds <- rep(NA, length(dch$R.indexes))

for (k in 1:length(dch$R.indexes)) {

    aa <- dch$R.indexes[[k]]
    R.prop.sds[k] <- 0.02
}

print(system.time(run.MCMC(dch, to.update, 2000, every=1000, burn.in=TRUE)))


for (k in 1:50) {
    
    run.MCMC(dch, to.update, 20000, every=2500, thin=20)
    
    cat("R jumps: ")
    print(round(rowMeans(simplify2array(dch$R.jumps.chain)), 2))
    cat("\n")
    
    pdf(file=paste("trace_plots/trace_plots_data_products_",
                   model.name, ".pdf", sep=""), width=6.3, height=6.5)
    trace.plots(dch)
    dev.off()
    
    save(dch, file=paste("chains/data_products_corr_", model.name, ".RData", sep=""))
}

