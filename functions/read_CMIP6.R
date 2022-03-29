


read.CMIP6.monthly.anom.to.annual.avg <- function (info, num, rescale=FALSE,
                                                   delta=0,
                                                   long.names=FALSE, cores=1) {

    zz <- read.CMIP6(info, num, rescale, delta, long.names)
    
    out <- calc.seasonal.anoms(zz$tas, zz$date, first, last)
    
    gc()
    
    global.monthly.avg <- area.weighted.average(out$tas, zz$lla$areas, cores=cores)[1,]
    
    year <- as.numeric(substr(out$date, 1, 4))
    
    tas <- as.numeric(tapply(global.monthly.avg, year, mean))
    
    gc()
    
    list(year=year, tas.yearly=tas-mean(tas), model=info$model.name, run=info$runs[num])
}



read.CMIP6 <- function (info, num, rescale=FALSE, delta=0, long.names=FALSE) {

    d2 <- CMIP6.dates(info, num, rescale=rescale, delta=delta)
    
    lla <- CMIP6.lat.long.areas(info, num, long.names)
    
    tas <- CMIP6.tas(info, num, d2, lla)

    list(info=info, date=d2, lla=lla, tas=tas)
}



CMIP6.filenames <- function (model.name, input.path) {
    
    all.files <- dir(file.path(input.path, model.name), "*.nc")
    
    all.runs  <- paste("r", sapply(strsplit(sapply(strsplit(all.files, "_r"),
                                               function (x) x[2]), "_"),
                               function (x) x[1]), sep="")    
    
    runs <- unique(all.runs)

    files <- lapply(runs, function (rr) drop(all.files[grepl(rr, all.files)]))

    list(model.name=model.name,
         input.path=input.path,
         runs=runs,
         files=files)
}



CMIP6.dates <- function (info, num, rescale=FALSE, delta=0) {

    fs <- info$files[[num]]
    
    M <- length(fs)
    
    origins <- NULL
    days.since <- NULL
    
    for (j in 1:M) {
        
        ## Read the data frame
        nc <- nc_open(file.path(info$input.path, info$model.name, fs[j]))
        
        ## Locate the days since argument
        txt     <- capture.output(print(nc))
        line.num <- grep(" units: days since", txt)
        origins[j]   <- strsplit(txt[[line.num[1]]], " ")[[1]][16]
        
        days.since <- c(days.since, ncvar_get(nc, "time"))
        
        nc_close(nc)
    }
    
    if (M>1) {
        
        if (!all(origins==origins[1])) { return (NULL) }
    }

    if (!rescale) {
        
        as.Date(origins[1]) + days.since + delta
    } else {

        as.Date(origins[1]) + seq(0, by=30.43658, length=length(days.since) + delta)
    }
}




CMIP6.lat.long.areas <- function (info, num, long.names=FALSE) {
    
    fs <- info$files[[num]]
    
    ## Read the data frame
    nc <- nc_open(file.path(info$input.path, info$model.name, fs[1]))
        
    ## Latitude
    if (long.names) {
        lat  <- ncvar_get(nc, "latitude")
    } else {
        lat  <- ncvar_get(nc, "lat")
    }       
    
    ## Longitude
    if (long.names) {
        long0 <- ncvar_get(nc, "longitude")
    } else {
        long0 <- ncvar_get(nc, "lon")
    }
    long1 <- ifelse(long0>180, long0-360, long0)    
    olong <- order(long1)    
    long  <- sort(long1)

    areas <- calc.area.matrix(long, lat)

    nc_close(nc)

    list(lat=lat, long=long, olong=olong, areas=areas)
}




CMIP6.tas <- function (info, num, dates, lla) {

    fs <- info$files[[num]]
    
    M <- length(fs)

    tas <- array(NA, c(length(lla$long), length(lla$lat), length(dates)))

    r <- 1

    for (j in 1:M) {

        if (j%%25==0) cat(round(j/M*100), "% ")
        
        ## Read the data frame
        nc <- nc_open(file.path(info$input.path, info$model.name, fs[j]))

        N <- length(ncvar_get(nc, "time"))

        tas[,,r+(0:(N-1))] <- ncvar_get(nc, "tas")[lla$olong,,] - 273.15

        r <- r + N

        nc_close(nc)
    }

    tas
}



