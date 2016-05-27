# wfx.r

require(Rcpp)
require(RcppParallel)
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

yy <- moduleFuncTest(1:10, 1:10, 3.0)

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

sourceCpp("wfx/wfx5.cpp")

sourceCpp("wfx/wfx6.cpp")

sourceCpp("wfx/wfx7.cpp")

sourceCpp("wfx/wfx8.cpp")

sourceCpp("wfx/wfx9.cpp")

eval(parse("app/util.r"))

xx <- load_zip_dat("app/dat/dzip2012.csv")

yy <- yg::CJ.dt(xx[zip %in% sample(zip, 1000)], xx[zip %in% sample(zip, 1000)])

microbenchmark::microbenchmark(dist_wlatlng(yy$lat, yy$lng, yy$i.lat, yy$i.lng),
                               distSgl_wlatlng_cpp(yy$lat, yy$lng, yy$i.lat, yy$i.lng),
                               distRpl_wlatlng_cpp(yy$lat, yy$lng, yy$i.lat, yy$i.lng),
                               times = 10)
# Unit: milliseconds
# expr       min        lq      mean    median        uq       max neval
# dist_wlatlng(yy$lat, yy$lng, yy$i.lat, yy$i.lng) 282.05968 294.77796 366.24650 339.18963 374.11749 578.42241    10
# distSgl_wlatlng_cpp(yy$lat, yy$lng, yy$i.lat, yy$i.lng) 265.01000 297.73562 356.41159 317.82904 371.55837 550.59845    10
# distRpl_wlatlng_cpp(yy$lat, yy$lng, yy$i.lat, yy$i.lng)  23.24352  23.54936  25.78948  26.19947  27.15631  28.65051    10



