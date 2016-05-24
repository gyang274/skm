# skm package test file

library(skm)


# test on class skm and skmSolution
mx = matrix(sample(100, 45), 5, 9)

s_init = c(0, 1)

s_must = 2

max_it = 10L

a_skm <- new(skm, mx)

a_skmSolution <- a_skm$skm_cpp(s_init, s_must, max_it)

a_skmSolution$show()

a_skmSolution <- a_skm$skm_cpp(c(1,3,4), integer(0L), max_it)

a_skmSolution$show()


# test on class skmRpl and skm_mlp_cpp
mx = matrix(sample(100, 45), 5, 9)

skm_mlp_cpp(mx, 2, integer(0L), 10, 1)

