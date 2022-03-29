
##install.packages("viridis", repos="https://cran.wustl.edu")
library("viridis")
library("RColorBrewer")


base.cols <- viridisLite::turbo(50)[-c(18:30)]

base.cols[18+(0:3)] <- brewer.pal(8, "Greens")[5:8]


the.cols <- base.cols[match(group, unique(group))]


