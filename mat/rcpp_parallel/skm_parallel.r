
# <http://eizoo.hatenablog.com/entry/2015/12/28/190212>

library(RcppArmadillo)
library(RcppParallel)
library(Rcpp)

sourceCpp("skm_parallel.cpp")

x = matrix(rnorm(36), 6, 6)

skmRplTestFunc(x, 10)
