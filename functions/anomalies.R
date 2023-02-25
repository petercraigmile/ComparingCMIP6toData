
monthly.anoms <- function (years.and.months, values,
                           ref.first = years.and.months[1],
                           ref.last  = years.and.months[length(years.and.months)]) {

    months <- round((years.and.months - floor(years.and.months))*12)

    ref.sel <- years.and.months>=ref.first & years.and.months<=ref.last

    years.and.months.subset  <- years.and.months[ref.sel]
    values.subset <- values[ref.sel]
    months.subset <- months[ref.sel]

    monthly.means <- tapply(values.subset, months.subset, mean)

    monthly.anoms <- values - monthly.means[months+1]

    list(years.and.months = years.and.months,
         monthly.anoms    = monthly.anoms,
         monthly.means    = monthly.means,
         ref.first        = ref.first,
         ref.last         = ref.last)
}


monthly.to.year.anoms <- function (years.and.months, monthly.anoms) {

    yearly.anoms <- tapply(monthly.anoms, floor(years.and.months), mean)
    
    years        <- unique(floor(years.and.months))
    
    list(years        = years,
         yearly.anoms = yearly.anoms)
}

