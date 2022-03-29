
## ======================================================================
## Creates the global 58 CMIP6 series from the historial nc runs
## downloaded from
## from https://esgf-node.llnl.gov/search/cmip6/
##
## Further details about the data at
## https://pcmdi.llnl.gov/CMIP6/Guide/dataUsers.html
## ======================================================================


source("../functions/read_CMIP6.R")
source("../functions/calc_area_by_lat.R")
source("../functions/calc_seasonal_anoms.R")
source("../functions/area_weighted_average.R")

library(ncdf4) ## ncdf4 must be installed
library(parallel)

input.path  <- "~/Documents/zz_Work_Archive/Data/2021_data_CMIP6/"
output.path <- "../Derived_Data/CMIP6_models"

first <- 1880
last  <- 2014


## ======================================================================
## 40 CMIP6 models that do not need their dates adjusted
## NOTE: "FGOALS-g3" runs to 2016, but does not need their dates adjusted
##
## We carry out plenty of garbage collection using gc() to free memory
## aggressively.
## ======================================================================

the.names <- c("ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", "BCC-ESM1",
               "CAMS-CSM1-0", "CanESM5", "CAS-ESM2-0", "CMCC-CM2-SR5",
               "CNRM-CM6-1", "CNRM-ESM2-1", "FIO-ESM-2-0", "GFDL-ESM4",
               "GISS-E2-1-G", "GISS-E2-1-H", "IITM", "INM-CM4-8",
               "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", "MPI-ESM-1-2-HAM",
               "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3",
               "FGOALS-g3", "E3SM-1-0", "E3SM-1-1", "E3SM-1-1-ECA",
               "SAM0-UNICON", "CMCC-CM2-HR4", "CMCC-ESM2", "CIESM",
               "GISS-E2-2-H", "KIOST-ESM", "IPSL-CM5A2-INCA", "IPSL-CM6A-LR-INCA",
               "CNRM-CM6-1-HR", "CanESM5-CanOE", "AWI-CM-1-1-MR", "AWI-ESM-1-1-LR")

for (model.ind in 1:length(the.names)) {

    cat("\n", the.names[model.ind], ":\n")
               
    info <- CMIP6.filenames(the.names[model.ind], input.path)
    
    model <- mclapply(1:length(info$runs), function (num)
        read.CMIP6.monthly.anom.to.annual.avg(info, num, rescale=FALSE, delta=0, cores=2), mc.cores=4)
    
    save(model, file=file.path(output.path, paste(info$model.name, ".RData", sep="")))    
    gc()
}


## ======================================================================
## 5 CMIP models that do not need their dates adjusted, but need a lot
## of memory to load - thus we reduce the number of outer mc.cores
## ======================================================================

the.names <- c("EC-Earth3", "EC-Earth3-Veg", "EC-EARTH3-AerChem", "EC-Earth3-CC",
               "EC-Earth3-Veg-LR")    

for (model.ind in 1:length(the.names)) {

    cat("\n", the.names[model.ind], ":\n")
               
    info <- CMIP6.filenames(the.names[model.ind], input.path)
    
    model <- mclapply(1:length(info$runs), function (num)
        read.CMIP6.monthly.anom.to.annual.avg(info, num, rescale=FALSE, delta=0, cores=7), mc.cores=1)
    
    save(model, file=file.path(output.path, paste(info$model.name, ".RData", sep="")))

    gc()
}


## ======================================================================
## 8 CMIP models that have the origin data set incorrectly.
## We need to manually move the date origin by 448 days in each case.
## ======================================================================

the.names <- c("CESM2", "CESM2-WACCM-FV2", "FGOALS-f3-L", "NorCPM1",
               "NorESM2-LM", "NorESM2-MM", "TaiESM1", "CESM2-FV2")

for (model.ind in 1:length(the.names)) {

    cat("\n", the.names[model.ind], ":\n")
               
    info <- CMIP6.filenames(the.names[model.ind], input.path)
    
    model <- mclapply(1:length(info$runs), function (num)
        read.CMIP6.monthly.anom.to.annual.avg(info, num, rescale=FALSE, delta=448, cores=2), mc.cores=4)
    
    save(model, file=file.path(output.path, paste(info$model.name, ".RData", sep="")))

    gc()
}

    
## ======================================================================
## 1 CMIP model that has the origin data set incorrectly.
## We need to manually move the date origin by 448 days in each case.
## Also, the lat/long variable names are different from the other datasets.
## ======================================================================

the.names <- c("MCM-UA-1-0")

for (model.ind in 1:length(the.names)) {

    cat("\n", the.names[model.ind], ":\n")
               
    info <- CMIP6.filenames(the.names[model.ind], input.path)
    
    model <- mclapply(1:length(info$runs), function (num)
        read.CMIP6.monthly.anom.to.annual.avg(info, num,
                                              rescale=FALSE, delta=448,
                                              long.names=TRUE,
                                              cores=2), mc.cores=4)
    
    save(model, file=file.path(output.path, paste(info$model.name, ".RData", sep="")))

    gc()
}




## ======================================================================
## 4 CMIP models that have a broken data structure, which we have to
## reconstruct.
## ======================================================================

the.names <- c("HadGEM3-GC31-LL", "HadGEM3-GC31-MM", "KACE-1-0-G", "UKESM1-0-LL")

for (model.ind in 1:length(the.names)) {

    cat("\n", the.names[model.ind], ":\n")
               
    info <- CMIP6.filenames(the.names[model.ind], input.path)
    
    model <- mclapply(1:length(info$runs), function (num)
        read.CMIP6.monthly.anom.to.annual.avg(info, num, rescale=TRUE, delta=0, cores=2), mc.cores=4)
    
    save(model, file=file.path(output.path, paste(info$model.name, ".RData", sep="")))    
}

gc()



