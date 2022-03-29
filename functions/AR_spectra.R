
dB <- function (x) {
    10 * log10(x)
}

ar.sdf <- function (freqs, phi, sigma2 = 1, delta.t = 1) 
{
    ws <- -2 * pi * delta.t * freqs
    js <- seq(length(phi))
    reals <- sapply(ws, function(w, js, phi) 1 - sum(phi * cos(js * 
        w)), js = js, phi = phi)
    imags <- sapply(ws, function(w, js, phi) sum(phi * sin(js * 
        w)), js = js, phi = phi)
    (sigma2 * delta.t)/(reals * reals + imags * imags)
}
