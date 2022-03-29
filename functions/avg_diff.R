
avg.diff <- function (temp, year,
                      from = c(1880, 1899),
                      to = c(1995, 2014)) {

    sel1 <- year >= from[1] & year <= from[2]
    sel2 <- year >= to[1] & year <= to[2]

    mean(temp[sel2]) - mean(temp[sel1])
}
