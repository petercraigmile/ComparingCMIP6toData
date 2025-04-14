
## To create posterior_summaries_AR4_basis8.RData run this file in R:

x1 <- readBin("posterior_summaries_AR4_basis8.RDataaa", "raw", 200000000)
x2 <- readBin("posterior_summaries_AR4_basis8.RDataab", "raw", 200000000)

writeBin(c(x1, x2), "posterior_summaries_AR4_basis8.RData")

