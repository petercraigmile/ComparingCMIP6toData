quantilefun =
    function(y)
    approxfun(seq(0, 1, length = length(y)), sort(y),
              yleft = NA, yright = NA)

percentilefun =
    function(y)
    approxfun(sort(y), seq(0, 1, length = length(y)),
              yleft = 0, yright = 1)

qqnorm =
    function(y, pch = 20,
             xlab = "Standard Normal Quantiles",
             ylab = "Sample Quantiles",line=T, ...)
    {
        y = sort(na.omit(y))
        n = length(y)
        p = (1:length(y) - .5)/length(y)
        k = .895 / (sqrt(n) * (1 - .01 / sqrt(n) + .85 / n))
        l = suppressWarnings(qnorm(p - k))
        q = qnorm(p)
        u = suppressWarnings(qnorm(p + k))
        plot(q, y, xlim = range(l, q, u, na.rm = TRUE),
             xlab = xlab, ylab = ylab, pch = pch, ...)
        lines(l, y, col = "darkgray")
        lines(u, y, col = "darkgray")
        if(line)abline(0,sd(y),col="red")
    }

qqplot =
    function(x, y, pch = 20,
             xlab = "x Quantiles",
             ylab = "y Quantiles",
             xlim=range(sort(na.omit(x))),cut=1.36,
             line=T,
             main = NULL, ...)
    {
        x = sort(na.omit(x))
        y = sort(na.omit(y))
        qy = quantilefun(y)
        m = length(x)
        n = length(y)
        N = m + n
        M = m * n / N
        K = cut
        p = (1:m - 1)/(m - 1)
        yq = qy(p)
        yl = qy(p - K/sqrt(M))
        yu = qy(p + K/sqrt(M))
        nn=length(x[x<xlim[2]])
        plot(x, yq, pch = pch,
             xlim = xlim,
             ylim = range(yq[1:nn], yl[1:nn], yu[1:nn], na.rm = TRUE),
             xlab = xlab, ylab = ylab, main = main, ...)
        lines(x, yl, col = "darkgray")
        lines(x, yu, col = "darkgray")
        if(line)abline(0,1,col="red")
    }

shiftplot =
    function(x, y, pch = 20,
             xlab = "x Quantiles",
             ylab = "y Quantiles",cut=1.36,
             xlim=range(sort(na.omit(x))),line=T,
             main = NULL, ...)
    {
        x = sort(na.omit(x))
        y = sort(na.omit(y))
        qy = quantilefun(y)
        m = length(x)
        n = length(y)
        N = m + n
        M = m * n / N
        K = cut
        p = (1:m - 1)/(m - 1)
        yq = qy(p) - x
        yl = qy(p - K/sqrt(M)) - x
        yu = qy(p + K/sqrt(M)) - x
        nn=length(x[x<xlim[2]])
        plot(x, yq, pch = pch,
              xlim = xlim,
             ylim = range(yq[1:nn], yl[1:nn], yu[1:nn], na.rm = TRUE),
             xlab = xlab, ylab = ylab, main = main, ...)
        lines(x, yl, col = "darkgray")
        lines(x, yu, col = "darkgray")
        if(line)abline(h=0,col="red")
        ## I moved this line...
        invisible(list(values=x,shiftfun=yq,lower=yl,upper=yu))
    }

