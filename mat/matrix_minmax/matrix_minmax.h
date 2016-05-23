#ifndef __MATRIXMINMAX__
#define __MATRIXMINMAX__


#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' mtx_min_val: min value of a matrix
// [[Rcpp::export]]
double mtx_min_val(NumericMatrix m) {

  double v = std::numeric_limits<double>::max();

  for (int i = 0; i < m.nrow(); i++ ) {

    for (int j = 0; j < m.ncol(); j++) {

      v = m(i, j) < v ? m(i, j) : v;

    }

  }

  return v;

}

//' mtx_min_val_wlmt: min value of a matrix within a limit range
// [[Rcpp::export]]
double mtx_min_val_wlmt(NumericMatrix m, IntegerVector wlmt_row, IntegerVector wlmt_col) {

  double v = std::numeric_limits<double>::max();

  IntegerVector::iterator it, jt;

  for ( it = wlmt_row.begin(); it != wlmt_row.end(); it ++ ) {

    for ( jt = wlmt_col.begin(); jt != wlmt_col.end(); jt++ ) {

      v = m(*it, *jt) < v ? m(*it, *jt) : v;

    }

  }

  return v;

}

//' mtx_min_idx: index of min value of a matrix
//' @note: cpp use index start from 0 - r use index start from 1
//' @note: also in case of equal std:min/max take the first seen
// [[Rcpp::export]]
IntegerVector mtx_min_idx(NumericMatrix m) {

  double v = std::numeric_limits<double>::max();

  IntegerVector idx(2);

  for (int i = 0; i < m.nrow(); i++ ) {

    for (int j = 0; j < m.ncol(); j++) {

      if ( m(i, j) < v ) { v = m(i, j); idx = IntegerVector::create(i, j); }

    }

  }

  return idx;

}

//' mtx_min_idx_lmt: index of min value of a matrix in limit rng
//' @param wlmt_row: limit search within row on indices wlmt_row
//' @param wlmt_col: limit search within col on indices wlmt_col
//' @note return the index of min value corresponding to m index
// [[Rcpp::export]]
IntegerVector mtx_min_idx_wlmt(NumericMatrix m, IntegerVector wlmt_row, IntegerVector wlmt_col) {

  double v = std::numeric_limits<double>::max();

  IntegerVector idx(2);

  IntegerVector::iterator it, jt;

  for ( it = wlmt_row.begin(); it != wlmt_row.end(); it ++ ) {

    for ( jt = wlmt_col.begin(); jt != wlmt_col.end(); jt++ ) {

      if ( m(*it, *jt) < v ) { v = m(*it, *jt); idx = IntegerVector::create(*it, *jt); }

    }

  }

  return idx;

}


//' mtx_max_val: max value of a matrix
// [[Rcpp::export]]
double mtx_max_val(NumericMatrix m) {

  double v = std::numeric_limits<double>::min();

  for (int i = 0; i < m.nrow(); i++ ) {

    for (int j = 0; j < m.ncol(); j++) {

      v = m(i, j) > v ? m(i, j) : v;

    }

  }

  return v;

}

//' mtx_max_val_wlmt: max value of a matrix within a limit range
// [[Rcpp::export]]
double mtx_max_val_wlmt(NumericMatrix m, IntegerVector wlmt_row, IntegerVector wlmt_col) {

  double v = std::numeric_limits<double>::min();

  IntegerVector::iterator it, jt;

  for ( it = wlmt_row.begin(); it != wlmt_row.end(); it ++ ) {

    for ( jt = wlmt_col.begin(); jt != wlmt_col.end(); jt++ ) {

      v = m(*it, *jt) > v ? m(*it, *jt) : v;

    }

  }

  return v;

}

//' mtx_max_idx: index of max value of a matrix
// [[Rcpp::export]]
IntegerVector mtx_max_idx(NumericMatrix m) {

  double v = std::numeric_limits<double>::min();

  IntegerVector idx(2);

  for (int i = 0; i < m.nrow(); i++ ) {

    for (int j = 0; j < m.ncol(); j++) {

      if ( m(i, j) > v ) { v = m(i, j); idx = IntegerVector::create(i, j); }

    }

  }

  return idx;

}

//' mtx_min_idx_lmt: index of min value of a matrix in limit rng
// [[Rcpp::export]]
IntegerVector mtx_max_idx_wlmt(NumericMatrix m, IntegerVector wlmt_row, IntegerVector wlmt_col) {

  double v = std::numeric_limits<double>::min();

  IntegerVector idx(2);

  IntegerVector::iterator it, jt;

  for ( it = wlmt_row.begin(); it != wlmt_row.end(); it ++ ) {

    for ( jt = wlmt_col.begin(); jt != wlmt_col.end(); jt++ ) {

      if ( m(*it, *jt) > v ) { v = m(*it, *jt); idx = IntegerVector::create(*it, *jt); }

    }

  }

  return idx;

}


//' row_wise_min_val: min value of each row in a matrix
// [[Rcpp::export]]
NumericVector row_wise_min_val(NumericMatrix m) {

  NumericVector v(m.nrow());

  for (int i = 0; i < m.nrow(); i++) {

    NumericVector mivec = m(i, _);

    v(i) = *std::min_element(mivec.begin(), mivec.end());

  }

  return v;

}

//' row_wise_min_idx: index of min value of each row in a matrix
//' @note: cpp use index start from 0 - r use index start from 1
//' @note: also in case of equal std:min/max take the first seen
// [[Rcpp::export]]
IntegerVector row_wise_min_idx(NumericMatrix m) {

  IntegerVector v(m.nrow());

  for (int i = 0; i < m.nrow(); i++) {

    NumericVector mivec = m(i, _);

    v(i) = std::min_element(mivec.begin(), mivec.end()) - mivec.begin();

  }

  return v;

}

//' row_wise_max_val: max value of each row in a matrix
// [[Rcpp::export]]
NumericVector row_wise_max_val(NumericMatrix m) {

  NumericVector v(m.nrow());

  for (int i = 0; i < m.nrow(); i++) {

    NumericVector mivec = m(i, _);

    v(i) = *std::max_element(mivec.begin(), mivec.end());

  }

  return v;

}

//' row_wise_max_idx: index of max value of each row in a matrix
//' @note: cpp use index start from 0 - r use index start from 1
//' @note: also in case of equal std:min/max take the first seen
// [[Rcpp::export]]
IntegerVector row_wise_max_idx(NumericMatrix m) {

  IntegerVector v(m.nrow());

  for (int i = 0; i < m.nrow(); i++) {

    NumericVector mivec = m(i, _);

    v(i) = std::max_element(mivec.begin(), mivec.end()) - mivec.begin();

  }

  return v;

}

//' col_wise_min_val: min value of each col in a matrix
// [[Rcpp::export]]
NumericVector col_wise_min_val(NumericMatrix m) {

  NumericVector v(m.ncol());

  for (int i = 0; i < m.ncol(); i++) {

    NumericVector mivec = m(_, i);

    v(i) = *std::min_element(mivec.begin(), mivec.end());

  }

  return v;

}

//' col_wise_min_idx: index of min value of each col in a matrix
//' @note: cpp use index start from 0 - r use index start from 1
//' @note: also in case of equal std:min/max take the first seen
// [[Rcpp::export]]
IntegerVector col_wise_min_idx(NumericMatrix m) {

  IntegerVector v(m.ncol());

  for (int i = 0; i < m.ncol(); i++) {

    NumericVector mivec = m(_, i);

    v(i) = std::min_element(mivec.begin(), mivec.end()) - mivec.begin();

  }

  return v;

}

//' col_wise_max_val: max value of each col in a matrix
// [[Rcpp::export]]
NumericVector col_wise_max_val(NumericMatrix m) {

  NumericVector v(m.ncol());

  for (int i = 0; i < m.ncol(); i++) {

    NumericVector mivec = m(_, i);

    v(i) = *std::max_element(mivec.begin(), mivec.end());

  }

  return v;

}

//' col_wise_max_idx: index of max value of each col in a matrix
//' @note: cpp use index start from 0 - r use index start from 1
//' @note: also in case of equal std:min/max take the first seen
// [[Rcpp::export]]
IntegerVector col_wise_max_idx(NumericMatrix m) {

  IntegerVector v(m.ncol());

  for (int i = 0; i < m.ncol(); i++) {

    NumericVector mivec = m(_, i);

    v(i) = std::max_element(mivec.begin(), mivec.end()) - mivec.begin();

  }

  return v;

}

#endif // __MATRIXMINMAX__
