
## ======================================================================
## Creates the global 58 CMIP6 series from the historial nc runs
## downloaded from
## from https://esgf-node.llnl.gov/search/cmip6/
##
## Further details about the data at
## https://pcmdi.llnl.gov/CMIP6/Guide/dataUsers.html
##
## This R code merges the files created in
## "2_prep_CMIP6_from_historical.R" to create the summary CSV file
## "CMIP6_tas_historical_models.csv" and the R data object in
## "CMIP6_tas_historical.RData".
## We also produce some summary figures.
## ======================================================================

output.path <- "../Derived_Data/CMIP6_models"

first <- 1880
last  <- 2014


files <- dir(output.path, "*.RData")

CMIP6.init <- sapply(1:length(files), function (j) {

    load(file.path(output.path, files[j]))

    get("model")
})


out <- sapply(CMIP6.init, function (y) {
    sapply(y, function (x)
        paste(x$model, x$run, min(x$year), max(x$year), sep=","))
})



cat(unlist(out), sep="\n", file="../Derived_Data/CMIP6_tas_historical_models.csv")


CMIP6.names <- sapply(CMIP6.init, function (x) x[[1]]$model)

CMIP6.list <- simplify2array(sapply(CMIP6.init, function (mm)
    sapply(mm, function (x) as.numeric(x$tas.yearly))))

the.names <- unlist(sapply(CMIP6.init, function (mm)
    sapply(mm, function (x) x$model)))

## Make the EC-Earth3 model naming consistent
the.names <- gsub("EC-EARTH3", "ECEarth", the.names, fixed=TRUE)
the.names <- gsub("EC-Earth3", "ECEarth", the.names, fixed=TRUE)

the.runs <- unlist(sapply(CMIP6.init, function (mm)
    sapply(mm, function (x) x$run)))

min.years <- unlist(sapply(CMIP6.init, function (mm)
    sapply(mm, function (x) min(x$year))))

max.years <- unlist(sapply(CMIP6.init, function (mm)
    sapply(mm, function (x) max(x$year))))


## check that all runs go from 'first' to 'last'
print(all(max.years==last) & all(min.years==first))



nms <- unique(the.names)

 nums <- rep(NA, length(the.names))
for (k in 1:length(nms)) {

     nums[the.names==nms[k]] <- k
}

## check number of rows are the same first!
print(sapply(CMIP6.list, dim)[1,])

CMIP6.data <- CMIP6.list[[1]]
for (k in 2:length(CMIP6.list)) {

    CMIP6.data <- cbind(CMIP6.data, CMIP6.list[[k]])
}





CMIP6 <- list(years       = first:last,
              model.anoms = CMIP6.data,
              model.names = the.names,
              runs        = the.runs,
              nums        = nums)


save(CMIP6, file="../Derived_Data/CMIP6_tas_historical.RData")


## Show distributions of run parameters

print(table(sapply(strsplit(the.runs, "i1"), function (x) x[2])))



## ======================================================================
## Produce some summary figures
## ======================================================================

pdf(file="figures/CMIP6_global_tas.pdf", width=6.3, height=2.5)
par(mfrow=c(1,1), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

matplot(CMIP6$years, CMIP6$model.anoms, type="l",
        col=CMIP6$nums, lty=1,
        xlab="year",
        ylab=expression(paste("Global average temperature (",degree,"C)" )))

dev.off()


num.models <- length(unique(CMIP6$model.names))

pdf(file="figures/CMIP6_global_tas_by_model.pdf", width=6.3, height=6)
par(mfrow=c(5,4), cex=0.6, mar=c(2.2,2.2,1,0.5), mgp=c(1.8,0.5,0), bty="L")

for (k in 1:num.models) {
    
    sel <- CMIP6$ nums == k    
    
    matplot(CMIP6$years,
            CMIP6$model.anoms[,sel], type="l",
            col=CMIP6$nums[sel], lty=1,
            ylim=c(-0.8, 1.5), main="", xlab="", ylab="")

    mtext(CMIP6$model.names[sel][1], side=3, line=0, cex=0.7)
}

dev.off()
