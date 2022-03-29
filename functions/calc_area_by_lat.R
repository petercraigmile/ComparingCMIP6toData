
calc.area.by.lat <- function (lat, w, h) {
    ## ======================================================================
    ## Calculate the area of a grid box centered at latitude 'lat' of
    ## width 'w' and height, 'h', in degrees.
    ## See http://mathforum.org/library/drmath/view/63767.html
    ## ======================================================================

    ## Equatorial radius 
    ## R <- 6378.137

    ## Use the mean radius instead
    R <- 6371.0088
    
    R^2 * abs(sin((lat-h/2)*pi/180)-sin((lat+h/2)*pi/180)) * abs(w) * pi / 180
}



calc.area.matrix <- function (long, lat) {
    ## ======================================================================
    ## Calculate the areas using the lat/long information in the
    ## vectors long and lat.
    ##
    ## Assumes that both the long and lat values are equally spaced.
    ##
    ## Output: matrix of areas of dimension length(long) x length(lat).
    ## ======================================================================

    ## Calculate the areas    
    ww <- diff(long)[1]
    hh <- diff(lat)[1]
    
    areas <- matrix(NA, length(long), length(lat))
    
    for (j in 1:length(lat)) {
        
        areas[,j] <-  calc.area.by.lat(lat[j], ww, hh)
    }

    areas
}
