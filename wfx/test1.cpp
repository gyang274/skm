#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
IntegerVector accessR_matrixAttr(NumericMatrix x, CharacterVector s_init) {

  CharacterVector x_s_name(x.attr("s_name"));

  IntegerVector x_s_init(x_s_name.length());

  x_s_init.names() = x_s_name;

  x_s_init[s_init] = 1;

  Rcout << as<std::string>(wrap(x_s_name(1))) << " : " << std::endl << x << std::endl;

  return x_s_init;
}

// [[Rcpp::export]]
IntegerVector seqLenTest1(int n) {

  return seq_len(n) - 1;

}

// [[Rcpp::export]]
uvec seqLenTest2(int n) {

  return cumsum(ones<uvec>(n)) - 1 ;
}

// [[Rcpp::export]]
uvec seqLenTest3(int n) {

  uvec s(n);

  for (int i = 0; i < n; i++) { s(i) = i; }

  return s;
}

// [[Rcpp::export]]
uvec seqLenTest4(int n) {

  uvec s1 = cumsum(ones<uvec>(n)) - 1;

  uvec s2(s1.begin(), s1.size(), false);

  return s2;
}



// [[Rcpp::export]]
int matrixSubset() {

  vec v = randu<vec>(10);

  Rcout << "v: " << v << endl;
  Rcout << "min value is " << v.min() << endl;


  uword  index;
  double min_val = v.min(index);

  Rcout << "min value is " << min_val << " at index "<< index << endl;


  mat A = randu<mat>(5,5);

  uword  row;
  uword  col;

  Rcout << "A: " << A << endl;

  double min_val2 = A.max(row,col);

  Rcout << "max value " << min_val2 << " is at " << row << ',' << col << endl;

  colvec w = randu<colvec>(10,1);
  double x = max(w);

  Rcout << "max value of " << w << " is " << x << endl;

  mat    M = randu<mat>(10,10);

  rowvec a = max(M);
  rowvec b = max(M,0);
  colvec c = max(M,1);

  // element-wise maximum
  mat X = randu<mat>(5,6);
  mat Y = randu<mat>(5,6);
  mat Z = arma::max(X,Y);

  Rcout << "X: " << endl << X << endl;

  Rcout << "Y: " << endl << Y << endl;

  Rcout << "Z: " << endl << Z << endl;

  return 42;
}

/*** R
x <- matrix(rnorm(9), 3, 3)

x <- x %>%
  `attr<-`("s_name", c("a", "b", "c")) %>%
  `attr<-`("t_name", c("m", "f", "o"))

accessR_matrixAttr(x, "b")
*/

