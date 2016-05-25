#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::uvec speedTest1(arma::uword k, arma::uvec s_must) {

  // create s_init w s_must and k
  arma::uvec ulmt = arma::cumsum(arma::ones<arma::uvec>(k)) - 1;

  if ( s_must.size() > 0 ) {

    if ( s_must.size() < k ) {

      for (arma::uword i = 0; i < s_must.size(); i++) {

        ulmt = ulmt(arma::find(ulmt != s_must(i)));

      }

    } else {

      stop("skm_rgi_cpp: s_must must have length < k");

    }

  }

  arma::uvec s_init = arma::join_cols(s_must, RcppArmadillo::sample(ulmt, k - s_must.size(), false));

  return s_init;


}

// [[Rcpp::export]]
arma::uvec speedTest2(arma::uword k, arma::uvec s_must) {

  // create s_init w s_must and k
  arma::uvec ulmt = arma::linspace<arma::uvec>(0,  k - 1,  k);

  if ( s_must.size() > 0 ) {

    if ( s_must.size() < k ) {

      for (arma::uword i = 0; i < s_must.size(); i++) {

        ulmt = ulmt(arma::find(ulmt != s_must(i)));

      }

    } else {

      stop("skm_rgi_cpp: s_must must have length < k");

    }

  }

  arma::uvec s_init = arma::join_cols(s_must, RcppArmadillo::sample(ulmt, k - s_must.size(), false));

  return s_init;


}


// [[Rcpp::export]]
arma::uword col_min_idx1(const arma::colvec u, const arma::ucolvec wlmt) {

  arma::uword min_val_idx;

  // wlmt.size() == 0 ? u.min(min_val_idx) : ( u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx); );

  if ( wlmt.size() == 0 ) {

    u.min(min_val_idx);

  } else {

    u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx);

  }

  return min_val_idx;
}

// [[Rcpp::export]]
arma::uword col_min_idx2(const arma::colvec& u, const arma::ucolvec& wlmt) {

  arma::uword min_val_idx;

  // wlmt.size() == 0 ? u.min(min_val_idx) : ( u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx); );

  if ( wlmt.size() == 0 ) {

    u.min(min_val_idx);

  } else {

    u(wlmt).min(min_val_idx); min_val_idx = wlmt(min_val_idx);

  }

  return min_val_idx;
}


/*** R
# speedTest1(10, c(1,0))
# speedTest2(10, c(1,0))
#
# microbenchmark::microbenchmark(speedTest1(10, c(1,0)),
#                                speedTest2(10, c(1,0)),
#                                times = 1000)

x = rnorm(10000)
y = sample(0:9999, 1000)

microbenchmark::microbenchmark(col_min_idx1(x, y),
                               col_min_idx2(x, y),
                               times = 100)


*/
