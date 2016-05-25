// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

// First passing Armadillo matrices by value
void doStuff1(arma::mat c, arma::mat d){
  double x = c(1,1) + d(1,1);
}

// [[Rcpp::export]]
void twoMatrices1(NumericMatrix A, NumericMatrix B) {
  arma::mat M1(A.begin(), A.nrow(), A.ncol(), false);
  arma::mat M2(B.begin(), B.nrow(), B.ncol(), false);

  for (int i = 0; i < 100; i++){
    doStuff1(M1, M2);
  }
}

// Now passign Armadillo matrices by reference
void doStuff2(const arma::mat& c, const arma::mat& d){
  double x = c(1,1) + d(1,1);
}

// [[Rcpp::export]]
void twoMatrices2(NumericMatrix A, NumericMatrix B) {
  arma::mat M1(A.begin(), A.nrow(), A.ncol(), false);
  arma::mat M2(B.begin(), B.nrow(), B.ncol(), false);

  for (int i = 0; i < 100; i++){
    doStuff2(M1, M2);
  }
}

// [[Rcpp::export]]
void twoMatrices3(arma::mat c, arma::mat d) {

  double x;

  for (int i = 0; i < 100; i++){
    x = c(1, 1) + d(1, 1);
  }

}


// [[Rcpp::export]]
void twoMatrices4(arma::mat& c, arma::mat& d) {

  double x;

  for (int i = 0; i < 100; i++){
    x = c(1, 1) + d(1, 1);
  }

}

/*** R
library(RcppArmadillo)
library(rbenchmark)

x = matrix(1,100,100)
y = matrix(1,100,100)

res <- benchmark(twoMatrices1(x, y), twoMatrices2(x, y),
                 twoMatrices3(x, y), twoMatrices4(x, y),
                 columns = c("test", "replications", "elapsed", "relative"),
                 order="relative", replications=1000)
print(res)

*/
