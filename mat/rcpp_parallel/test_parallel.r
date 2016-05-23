library(Rcpp)
library(inline)

sourceCpp("test_parallel.cpp")

testmything = function() {
  calculateMyThingParallel()
}

if(TRUE) {
  for(i in 1:20) {
    testmything()
  }
}