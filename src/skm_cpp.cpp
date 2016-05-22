#include "matrix_minmax.h"

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


using namespace Rcpp;
// using namespace arma;

// http://stackoverflow.com/questions/21944695/rcpparmadillo-and-arma-namespace

// cann't use <using namespace arma> in developing r-package with RcppArmadillo
// because Rcpp when it compiles C++ source files RcppExports.cpp does not copy
// over the using namespace arma statements into there.

// since different files cab use different namespace, so Rcpp attributes parser
// can't just copy all the using namespace ... statements into RcppExports.cpp,
// so it just defaults to using the Rcpp namespace. If Rcpp just copied all the
// using namespace statements willy nilly into RcppExports.cpp surely conflict.

// fix is either to explicitly prefix arma:: or modify the RcppExports.cpp file
// and add using namespace arma; at the top - remember you'd have to do it each
// time after calling compileAttributes(). - Kevin Ushey

//' skm_minmax_cpp: skm via min-max on in cpp.
//' skm_minmax_cpp with an input m x n matrix: objective is to select n of m st
//' minimize sum(min(<i, j> where i <1..n> and j <1..n> each use <1..n> once)).
//' so in case m <= n it simply select all m - should always be apply on matrix
//' with m > n - it is designed as a expectation step in skm_cpp on updating s.
//' it select i in <1..m> such that i has the colwise_min_idx on column j where
//' j has max difference of (colwise_max_val - colwise_min_val), it then remove
//' row i col j from matrix and repeat.
//' example skm_minmax_cpp is superior in bouding worst case compare to greedy:
//' x = [1 100; 4 200; 2 400; 9 900]: greedy 1 then 200, min-max 100 then 2, so
//' greedy give [1 100; 4 200] with 201 and minmax give [1 100; 2 400] with 102
// [[Rcpp::export]]
arma::uvec skm_minmax_cpp(arma::mat x) {

  if (x.n_rows < x.n_cols) { stop("x must a matrix m x n with m >= n.\n"); }

  arma::uvec s = arma::zeros<arma::uvec>(x.n_cols);

  arma::uvec t = arma::zeros<arma::uvec>(x.n_cols);

  arma::uvec ulmt = arma::cumsum(arma::ones<arma::uvec>(x.n_rows)) - 1;

  arma::uvec vlmt = arma::cumsum(arma::ones<arma::uvec>(x.n_cols)) - 1;

  for (arma::uword i = 0; i < x.n_cols; i++) {

    arma::vec v = arma::zeros<arma::vec>(x.n_cols);

    for (arma::uvec::iterator jt = vlmt.begin(); jt != vlmt.end(); jt++) {

      v(*jt) = col_rgn_val(x.col(*jt), ulmt);

    }

    // Rcout << "v: " << v.t() << endl;

    t(i) = col_max_idx(v, vlmt);

    // Rcout << "t: " << t.t() << endl;

    vlmt = vlmt(find(vlmt != t(i)));

    // Rcout << "vlmt: " << vlmt.t() << endl;

    s(i) = col_min_idx(x.col(t(i)), ulmt);

    // Rcout << "s: " << s << endl;

    ulmt = ulmt(find(ulmt != s(i)));

    // Rcout << "ulmt: " << ulmt << endl;

  }

  // Rcout << "objective: " << std::endl;

  // Rcout << "t: " << t.t() << std::endl;

  // Rcout << "s: " << s << std::endl;

  // double o = 0;

  // for (arma::uword i = 0; i < x.n_cols; i++) { o += x(s(i), t(i));  }

  // Rcout << "o: " << o << std::endl;

  return s;
}

//' skm_cpp: skm core algorithm in cpp.
//' @param x: matrix with s<source> as row t<target> as col and d<dist> in cell
//' @return
// [[Rcpp::export(name=".skm_cpp")]]
int skm_cpp(arma::mat x) {

  return 42;
}
