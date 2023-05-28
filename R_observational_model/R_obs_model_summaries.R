
library(splines)
library(nlme)
library(mvtnorm)
library(excursions)

source("../functions/figure_utils.R")
source("../functions/plot_CI.R")
source("../functions/AR.R")
source("../functions/R_obs_model_post_summaries.R")

load("../Derived_Data/data_products.RData")


mdl.names <- c("AR4_basis6",
               "AR2_basis6",
               "AR4_basis8",
               "AR2_basis8",
               "AR4_basis8_v2",
               "AR4_basis4",
               "AR6_basis6")


trend.sdfs <- Y.summs <- dch.list <- list()

for (k in 4) {## 1:length(mdl.names)) {

    mn <- mdl.names[k]
    
    ## Loading in the data frames
    load(paste("chains/data_products_corr_", mn, ".RData", sep=""))
    dch.list[[k]] <- dch

    Y.summs[[k]] <- summarize.Y(dch)

    trend.sdfs[[k]] <- summarize.trend.sdfs(dch)
}




for (k in 4) {## 1:length(mdl.names)) {


#for (k in 1:length(mdl.names)) {

    mn <- mdl.names[k]
    
    pdf(file=paste("figures/BHM_data_products_", mn, ".pdf", sep=""),
        width=6, height=2.5)

    post.Y.plot(Y.summs[[k]])

    dev.off()

    pdf(file=paste("figures/trends_sdf_", mn, ".pdf", sep=""),
        width=6, height=2.5)

    post.trends.sdfs(trend.sdfs[[k]])

    dev.off()


    pdf(paste(file="figures/discrepancies_", mn, ".pdf", sep=""),
        width=6, height=4.5)
    par(mfrow=c(3,2), cex=0.65, mar=c(2.8,2.8,1,0.8), mgp=c(1.6,0.4,0), bty="L") 
    
    plot.discrepancy(dch.list[[k]])
    
    dev.off()
}





for (k in 4) {## 1:length(mdl.names)) {

    mn <- mdl.names[k]

    sink(paste("figures/R_summary_", mn, ".txt", sep=""))
    
    R.summ <- R.post.summary(dch.list[[k]])
    
    cat(mn, "\n\n")
    print(round(R.summ$R.mean, 2))
    cat("\n")
    print(round(R.summ$R.025, 2))
    cat("\n")
    print(round(R.summ$R.975, 2))
    
    sink()
}
    
