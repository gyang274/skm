#ifndef __MATRIXMINMAX__
#define __MATRIXMINMAX__

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;
using namespace RcppParallel;


//' col_min_idx: colvec min value index within limited range
//' @param wlmt: a limit search on colvec on indices within wlmt
//' @return return an index of min value w.r.t to original index
//' @note cpp use index start from 0 vs r use index start from 1
//' @note in case of equal std:min/std:max take first index seen
// [[Rcpp::export]]
arma::uword col_min_idx(const arma::colvec u, const arma::ucolvec wlmt) {

  arma::uword min_val_idx;

  // wlmt.size() == 0 ? u.min(min_val_idx) : ( u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx); );

  if ( wlmt.size() == 0 ) {

    u.min(min_val_idx);

  } else {

    u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx);

  }

  return min_val_idx;
}

//' col_max_idx: colvec max value index within limited range
//' @param wlmt: a limit search on colvec on indices within wlmt
//' @return return an index of max value w.r.t to original index
//' @note cpp use index start from 0 vs r use index start from 1
//' @note in case of equal std:min/std:max take first index seen
// [[Rcpp::export]]
arma::uword col_max_idx(const arma::colvec u, const arma::ucolvec wlmt) {

  arma::uword max_val_idx;

  // wlmt.size() == 0 ? u.max(max_val_idx);: ( u(wlmt).max(max_val_idx); max_val_idx = wlmt(max_val_idx); );

  if ( wlmt.size() == 0 ) {

    u.max(max_val_idx);

  } else {

    u(wlmt).max(max_val_idx); max_val_idx = wlmt(max_val_idx);

  }

  return max_val_idx;
}

//' col_min_val: colvec min value within limited range
// [[Rcpp::export]]
double col_min_val(const arma::colvec u, const arma::ucolvec wlmt) {

  return wlmt.size() > 0 ? u(wlmt).min() : u.min() ;

}

//' col_max_val: colvec max value within limited range
// [[Rcpp::export]]
double col_max_val(const arma::colvec u, const arma::ucolvec wlmt) {

  return wlmt.size() > 0 ? u(wlmt).max() : u.max() ;
}

//' col_rgn_val: colvec diff max min value within limited range
// [[Rcpp::export]]
double col_rgn_val(const arma::colvec u, const arma::ucolvec wlmt) {

  return wlmt.size() > 0 ? u(wlmt).max() - u(wlmt).min() : u.max() - u.min() ;
}

//' rgn_vec: colvec range from s to z or from 0 to s when z NIL
// [[Rcpp::export]]
arma::uvec rgn_vec(arma::uword s, arma::uword z = 0) {

  arma::uword min_val = s < z ? s : z;

  arma::uword max_val = s > z ? s : z;

  arma::uword n = max_val - min_val;

  arma::uvec v(n);

  for (arma::uword i = 0; i < n; i++) { v(n) = min_val + i; }

  return v;

}


#endif // __MATRIXMINMAX__


