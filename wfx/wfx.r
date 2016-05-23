# wfx.r

require(Rcpp)
require(RcppArmadillo)

sourceCpp("wfx/wfx1.cpp")

microbenchmark::microbenchmark(
  t1 = seqLenTest1(100), t2 = seqLenTest2(100),
  t3 = seqLenTest3(100), t4 = seqLenTest4(100))

sourceCpp("wfx/wfx2.cpp")

xx <- new(skmSolution, c(1:10), sample(10), 77.85)

xx$s

xx[["t"]]

xx$o

sourceCpp("wfx/wfx3.cpp")

mx = matrix(sample(100, 28), 7, 4)

xx = skm_minmax_cpp(mx)

mx = matrix(sample(100, 45), 5, 9)

xx = skm_cpp(mx, c(1, 2, 4), integer(0L), max_it = 10)

xx = skm_cpp(mx, c(1, 2, 4), 0, max_it = 10)

mx = matrix(abs(rnorm(52 * 50000)), 52, 50000)

xx = skm_cpp(mx, 1L:10L, integer(0L), max_it = 1000)

rm(list = setdiff(ls(), "mx"))

sourceCpp("wfx/wfx4.cpp")

