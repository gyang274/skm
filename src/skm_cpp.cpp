#include "skm.h"
#include "matrix_minmax.h"

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


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

using namespace Rcpp;
// using namespace arma;


// skm::skm_minmax_cpp: skm via min-max on in cpp - subroutine of skm::skm_cpp
// skm_minmax_cpp with an input m x n matrix: objective is to select n of m st
// minimize sum(min(<i, j> where i <1..n> and j <1..n> each use <1..n> once)).
// so in case m <= n it simply select all m - should always be apply on matrix
// with m > n - it is designed as a expectation step in skm_cpp on updating s.
// it select i in <1..m> such that i has the colwise_min_idx on column j where
// j has max difference of (colwise_max_val - colwise_min_val), it then remove
// row i col j from matrix and repeat.
// example skm_minmax_cpp is superior in bouding worst case compare to greedy:
// x = [1 100; 4 200; 2 400; 9 900]: greedy 1 then 200, min-max 100 then 2, so
// greedy give [1 100; 4 200] with 201 and minmax give [1 100; 2 400] with 102
skmSolution skm::skm_minmax_cpp(const arma::mat x, const arma::uvec s_must) {

  if (x.n_rows < x.n_cols) { stop("x must a matrix m x n with m >= n.\n"); }

  arma::uvec s = arma::zeros<arma::uvec>(x.n_cols);

  arma::uvec t = arma::zeros<arma::uvec>(x.n_cols);

  arma::uvec ulmt = arma::cumsum(arma::ones<arma::uvec>(x.n_rows)) - 1;

  arma::uvec vlmt = arma::cumsum(arma::ones<arma::uvec>(x.n_cols)) - 1;

  // s_must must in s rslt
  if ( s_must.size() > 0 ) {

    if ( s_must.size() > x.n_cols ) {

      stop("skm_minmax_cpp: s_must must have length <= x.n_cols ...\n");

    }

    for (arma::uword i_init = 0; i_init < s_must.size(); i_init++) {

      s(i_init) = s_must(i_init);

      // if s_must(i_init) happens to select a row contains one or more
      // min_val of some col then select it achieve a max_val - min_val
      // benefit and vlmt should remove such col and when multiple cols
      // vlmt should remove the one with highest benefit w.r.t max-min.

      // more thoughts: even if s_must does not happend to select a row
      // contain one or more min_val, we should remove some one col wrt
      // benenfit max - val(on row s) because we don't want then select
      // another row s2 that is very close to s - e.g. s has 2nd min on
      // col t but s2 has 1st min on col t is then select - a real case
      // example would be we have two group from t, says, East and West
      // we put NJ 2nd best among East in s_must and then select NY 1st
      // instead we should remove East from consideration and select CA

      // Rcout << "push s_must into s: " << s_must(i_init) << std::endl;

      // arma::uvec v_min_idx = arma::zeros<arma::uvec>(x.n_cols);

      arma::vec v_bnt_val = arma::zeros<arma::vec>(x.n_cols);

      for (arma::uvec::iterator jt = vlmt.begin(); jt != vlmt.end(); jt++) {

        v_bnt_val(*jt) = col_max_val(x.col(*jt), ulmt) - x(s(i_init), *jt);

      }

      vlmt = vlmt(find(vlmt != col_max_idx(v_bnt_val, vlmt)));

      ulmt = ulmt(find(ulmt != s(i_init)));

    }

  }

  for (arma::uword i = s_must.size(); i < x.n_cols; i++) {

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

  // Rcout << "solution: " << std::endl;

  // Rcout << "s: " << s << std::endl;

  // Rcout << "t: " << t.t() << std::endl;

  // Rcout << "objective: " << std::endl;

  double o = 0;

  for (arma::uword i = 0; i < x.n_cols; i++) { o += x(s(i), t(i)); }

  // Rcout << "o: " << o << std::endl;

  return skmSolution(s, t, o);
}

// Rcpp attributes code that parses function declarations isn't able to parsing
// all syntactic forms of C++ but rather a subset. The default argument parsing
// is able to handle scalars, strings, and simple vector initializations but no
// more complex expressions like ucolvec() - J.J.Allaire <jj.allaire@gmail.com>

// workaourd? call function with argu v = ucolvec() and test with v.size() == 0

// skm::skm_cpp: solve skm via selective k-means.
// member function of class skm: w. mat x, uvec s_init, uvec s_must, int max_it
skmSolution skm::skm_cpp(arma::uvec s_init, arma::uvec s_must, arma::uword max_it) {

  arma::uvec s(s_init.begin(), s_init.size());

  arma::uvec t(x.n_cols);

  double o = std::numeric_limits<double>::max();

  // Rcout << "s : " << std::endl << s << std::endl;

  // Rcout << "t : " << std::endl << t.t() << std::endl;

  arma::uword num_it = 0;

  bool archive_optim = false;

  while ( !archive_optim && (num_it < max_it) ) {

    // minimization step - assign t into s

    // Rcout << "num_it : " << num_it << std::endl;

    for (arma::uword j = 0; j < x.n_cols; j++) {

      t(j) = col_min_idx(x.col(j), s);
    }

    // Rcout << "t : " << t.t() << std::endl;

    // aexpectation step - update s with t

    arma::mat gx(x.n_rows, s.size());

    for (arma::uword i = 0; i < s.size(); i++) {

      arma::uvec g = find(t == s(i));

      // Rcout << "g : " << g << std::endl;

      // sum(matrix, 0) row_wise_sum
      // sum(matrix, 1) col_wise_sum

      gx.col(i) = sum(x.cols(g), 1);

    }

    // Rcout << "gx :" << gx << std::endl;

    skmSolution xs = skm::skm_minmax_cpp(gx, s_must);

    s = xs.s; double xo = xs.o;

    // Rcout << "s : " << s << std::endl;

    // Rcout << "xo : " << xo << std::endl;

    if ( xo < o ) { o = xo; } else { o = xo; archive_optim = true; }

    num_it++;

  }

  // Rcout << "construct solution: " << std::endl;

  // s and o are set, only need to update t
  for (arma::uword j = 0; j < x.n_cols; j++) {

    t(j) = col_min_idx(x.col(j), s);

  }

  // Rcout << "s: " << s << std::endl;

  // Rcout << "t: " << t.t() << std::endl;

  // Rcout << "o: " << o << std::endl;

  return skmSolution(s, t, o);
}

// skm::skm_mlp: wrapper over skm_cpp - multiple runs and return best of all
skmSolution skm::skm_mlp(arma::uvec s_init, arma::uvec s_must, arma::uword max_it, arma::uword max_at) {

  arma::uvec s(2);

  arma::uvec t(1);

  double o = 99.87;

  return skmSolution(s, t, o);
}
