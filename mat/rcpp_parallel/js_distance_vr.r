# <http://gallery.rcpp.org/articles/parallel-distance-matrix/>
# <http://stackoverflow.com/questions/30791410/how-to-call-user-defined-function-in-rcppparallel>

# setwd(...)

# r version
js_distance <- function(mat) {
  kld = function(p,q) sum(ifelse(p == 0 | q == 0, 0, log(p/q)*p))
  res = matrix(0, nrow(mat), nrow(mat))
  for (i in 1:(nrow(mat) - 1)) {
    for (j in (i+1):nrow(mat)) {
      m = (mat[i,] + mat[j,])/2
      d1 = kld(mat[i,], m)
      d2 = kld(mat[j,], m)
      res[j,i] = sqrt(.5*(d1 + d2))
    }
  }
  res
}

# cpp version - serial verison
sourceCpp("js_distance_vs.cpp")

# cpp version - parallel verison
sourceCpp("js_distance_vp.cpp")

# cpp version - parallel + armadillo verison
# Convert RVector / RMatrix into arma::vec / arma::mat: as mentioned in RcppParallel Github, C++ Armadillo are thread-safe in struct's operator. 
sourceCpp("js_distance_vu.cpp")

# benchmark
# create a matrix
n  = 1000
m = matrix(runif(n*10), ncol = 10)
m = m/rowSums(m)

# ensure that serial and parallel versions give the same result
r_res <- js_distance(m)
rcpp_res <- rcpp_js_distance(m)
rcpp_parallel_res <- rcpp_parallel_js_distance(m)
rcpp_parallel_m_res <- rcpp_parallel_js_distance_modify(m, 10)
stopifnot(all(rcpp_res == rcpp_parallel_res))
stopifnot(all(rcpp_parallel_res - r_res < 1e-10)) ## precision differences

# compare performance
library(rbenchmark)
res <- benchmark(js_distance(m),
                 rcpp_js_distance(m),
                 rcpp_parallel_js_distance(m),
                 rcpp_parallel_js_distance_modify(m, 10),
                 replications = 3,
                 order="relative")
res[,1:4]

res2 <- benchmark(rcpp_parallel_js_distance(m),
                  rcpp_parallel_js_distance_modify(m, 32),
                  replications = 3,
                  order="relative")
res2[,1:4]

