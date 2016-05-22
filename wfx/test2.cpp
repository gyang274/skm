#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;


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

  Rcout << "objective: " << std::endl;

  Rcout << "t: " << t.t() << std::endl;

  Rcout << "s: " << s << std::endl;

  double o = 0;

  for (arma::uword i = 0; i < x.n_cols; i++) { o += x(s(i), t(i));  }

  Rcout << "o: " << o << std::endl;

  return s;
}


// Rcpp attributes code that parses function declarations isn't able to parsing
// all syntactic forms of C++ but rather a subset. The default argument parsing
// is able to handle scalars, strings, and simple vector initializations but no
// more complex expressions like ucolvec() - J.J.Allaire <jj.allaire@gmail.com>

// workaourd? call function with argu v = ucolvec() and test with v.size() == 0

// [[Rcpp::export]]
int skm_cpp(arma::mat x, const int k, const arma::ucolvec s_init,
            const arma::ucolvec s_must, const arma::uword max_it = 2) {

  arma::ucolvec s(s_init);

  arma::urowvec t(x.n_cols);

  Rcout << "s : " << std::endl << s << std::endl;

  Rcout << "t : " << std::endl << t << std::endl;

  arma::uword num_it = 0;

  while ( num_it < max_it ) {

    for (arma::uword j = 0; j < x.n_cols; j++) {

      t(j) = col_min_idx(x.col(j), s);
    }

    Rcout << "t : " << std::endl << t << std::endl;

    for (arma::uword i = 0; i < s.size(); i++) {

      // add to deal with repeated s...

      arma::uvec g = find(t == s(i));

      s(i) = col_min_idx(sum(x.cols(g), 1), arma::ucolvec());

      Rcout << "i : " << std::endl << i << std::endl;

      Rcout << "g : " << std::endl << g << std::endl;
    }

    Rcout << "s : " << std::endl << s << std::endl;

    num_it++;
  }


  return 42;
}
