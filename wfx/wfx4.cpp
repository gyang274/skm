// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>

using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

// random sample w. RcppArmadillo::sample

// [[Rcpp::export]]
NumericVector csample_num( NumericVector x,
                           int size,
                           bool replace,
                           NumericVector prob = NumericVector::create()
                             ) {
  NumericVector ret = RcppArmadillo::sample(x, size, replace, prob);
  return ret;
}

// [[Rcpp::export]]
int arma_sampe(arma::uvec x, arma::uword size, bool replace, arma::vec prob) {
  
  Rcout << "x-in:   " << x << std::endl;
  
  Rcout << "sample: " << RcppArmadillo::sample(as<IntegerVector>(wrap(x)), as<int>(wrap(size)), replace, as<NumericVector>(wrap(prob))) << std::endl;
  
  return 42;
}

// [[Rcpp::export]]
int arma_subset(arma::vec x, arma::uvec idx) {
  
  Rcout << "1: " << x(idx) << std::endl;
  
  arma::uvec anti_idx = arma::cumsum(arma::ones<arma::uvec>(x.n_rows)) - 1;
  
  for (arma::uword i = 0; i < idx.size(); i++) {
    
    anti_idx = anti_idx(find(anti_idx != idx(i)));
    
  }
  
  Rcout << "2: " << x(anti_idx) << std::endl;
  
  return 42;
}

// [[Rcpp::export]]
void arma_empty_vec(arma::uvec v) {
  
  Rcout << "empty? : " << v << endl;
  
  Rcout << "empty? : " << v.size() << endl;
  
}

// [[Rcpp::export]]
void call_empty_vec(int k) {
  
  if (k < 0) {
    
    arma_empty_vec(arma::uvec());
    
  } else {
    
    arma_empty_vec(arma::uvec(k));
  }
  
}


