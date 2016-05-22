# test.r

sourceCpp("wfx/test1.cpp")

sourceCpp("wfx/test2.cpp")


microbenchmark::microbenchmark(
  t1 = seqLenTest1(100), t2 = seqLenTest2(100),
  t3 = seqLenTest3(100), t4 = seqLenTest4(100))
