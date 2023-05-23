
## ======================================================================
## R code to create the data product time series used in the article
##
## P. F. Craigmile and P. Guttorp,
## A combined estimate of global temperature
##
## Contact: pfc@stat.osu.edu
## ======================================================================

source("../functions/subset_and_anom.R")

## The anomalies are calculated relative to the years 1880 - 2014.

first.year <- 1880
last.year  <- 2014




## ======================================================================
## Berkeley
##
##  "Uncertainties represent the 95% confidence interval for
##  statistical and spatial undersampling effects as well as ocean
##  biases."
##
## We recommended dividing the uncertainty values by 1.96 to obtain
## the standard error for the global mean temperatures.
## ======================================================================

## Variables:
##       Land + Ocean anomaly using air temperature above sea ice        Land + Ocean using water temperature below sea ice
## Year, Annual Anomaly, Annual Unc., Five-year Anomaly, Five-year Unc., Annual Anomaly, Annual Unc., Five-year Anomaly, Five-year Unc.

Berkeley.df <- read.table("Berkeley/Land_and_Ocean_summary_2022_10_18.txt",
                 skip=58, header=FALSE)

Berkeley <- subset.and.anom(years    = Berkeley.df[,1],
                            the.mean = Berkeley.df[,2],
                            the.vars = (Berkeley.df[,3]/1.96)^2,
                            first.year = first.year,
                            last.year = last.year)


## ======================================================================
## HadCRUT 5.0.1.0
## ======================================================================

df <- read.csv("HadCRUT.5.0.1.0/HadCRUT.5.0.1.0.analysis.component_series.global.annual.csv")

tas <- df$Anomaly..deg.C.

years <- df$Time

HadCRUT5.vars <- df$Total.uncertainty..1.sigma.^2

HadCRUT5 <- subset.and.anom(years    = years,
                            the.mean = tas,
                            the.vars = HadCRUT5.vars,
                            first.year = first.year,
                            last.year = last.year)


## ======================================================================
## NOAA
##
## Annual data (aravg.ann.*) :
## 1st column = year
## 2nd column = anomaly of temperature (K)
## 3rd column = total error variance (K**2)
## 4th column = high-frequency error variance (K**2)
## 5th column = low-frequency error variance (K**2)
## 6th column = bias error variance (K**2)
##
## NOTE:
## 4+5+6 = 3, except for the last year
## ======================================================================

NOAA.df <- read.table("NOAA/aravg.ann.land_ocean.90S.90N.v5.0.0.202012.asc.txt")

NOAA <- subset.and.anom(years    = NOAA.df[,1],
                        the.mean = NOAA.df[,2],
                        ##the.vars = NOAA.df[,4]) ## high freq
                        ##the.vars = NOAA.df[,5]) ## low freq
                        ##the.vars = NOAA.df[,6]) ## bias
                        the.vars = NOAA.df[,4]+NOAA.df[,5]+NOAA.df[,6],
                        first.year = first.year,
                        last.year = last.year)


## ======================================================================
## GISS
## ======================================================================

GISS.df <- read.csv("GISS/totalCI_ERA.csv")

GISS <- subset.and.anom(years    = GISS.df[,1],
                        the.mean = GISS.df[,2],
                        the.vars = (GISS.df[,3] / (1.96))^2,
                        first.year = first.year,
                        last.year = last.year)


## source("../functions/plot_CI.R")

## pdf(file="figures/GISS.pdf", width=6.3, height=2.5)
## par(mfrow=c(1,1), cex=0.75, mar=c(3.1,3.1,1,0.5), mgp=c(1.8,0.5,0), bty="L")

## plot(GISS$years, GISS$the.mean, type="n", ylim=c(-0.6, 1.2))

## plot.CI(GISS$years,
##         lower=GISS$the.mean-2*sqrt(GISS$the.vars),
##         upper=GISS$the.mean+2*sqrt(GISS$the.vars))

## lines(GISS$years, GISS$the.mean, type="l", pch=15, cex=0.5)

## dev.off()






## ======================================================================
## Cowtan and Way V2
## ======================================================================

df <- read.table("CowtanWayV2/had4_krig_annual_v2_0_0.txt", header=FALSE)

CW2 <- subset.and.anom(years    = df[,1],
                       the.mean = df[,2],
                       the.vars = df[,3]^2,
                       first.year = first.year,
                       last.year = last.year)



## ======================================================================
## Japan - JMA
## ======================================================================

JMA0 <- read.csv("JMA/year_wld.csv")

jyears <- JMA0[,1]
range(years)

JMA.mean <- JMA0[,2]

JMA.sds <- read.csv("JMA/JMA_SDs.csv")[,2]

JMA.initial <- list(years     = jyears,
                    the.mean = JMA.mean,
                    the.vars = JMA.sds^2)

## Now build the longer series for JMA (hard wired for JMA years - if
## the dataset changes, this code has to change!)

JMA <- subset.and.anom(years    = c(1880:1890, JMA.initial$years),
                       the.mean = c(rep(NA,11), JMA.initial$the.mean),
                       the.vars = c(rep(NA,11), JMA.initial$the.vars),
                       first.year = first.year,
                       last.year = last.year)






## ======================================================================
## Now combine the datasets and save to a RData file
## ======================================================================

years <- Berkeley$years

global.anoms <- cbind(Berkeley$the.mean,
                      HadCRUT5$the.mean,
                      NOAA$the.mean,
                      GISS$the.mean,
                      CW2$the.mean,
                      JMA$the.mean)

global.vars <- cbind(Berkeley$the.vars,
                     HadCRUT5$the.vars,
                     NOAA$the.vars,
                     GISS$the.vars,
                     CW2$the.vars,
                     JMA$the.vars)

data.prods.labels <- c("Berkeley",
                       "HadCRUT5",
                       "NOAA",
                       "GISS",                       
                       "CowtanWayV2",
                       "JMA")

data.prods <- list(years        = years,
                   global.anoms = global.anoms,
                   global.vars  = global.vars,
                   labels       = data.prods.labels)

save(data.prods,
     file="../Derived_Data/data_products.RData")
