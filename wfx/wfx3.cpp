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


// <http://www.cplusplus.com/doc/tutorial/classes/>
// expression	can be read as
// *x	    pointed to by x
// &x	    address of x
// x.y	  member y of object x
// x->y	  member y of object pointed to by x
// (*x).y	member y of object pointed to by x (equivalent to the previous one)
// x[0]   first object pointed to by x
// x[1]   second object pointed to by x
// x[n]   (n+1)th object pointed to by x
class skmSolution {

public:

  //constructor
  skmSolution(arma::uvec s_, arma::uvec t_, double o_) : s(s_) , t(t_), o(o_) {}

  arma::uvec s; arma::uvec t; double o;
};

RCPP_EXPOSED_CLASS(skmSolution);

// <http://dirk.eddelbuettel.com/code/rcpp/Rcpp-modules.pdf>
// expose class into R using RCPP_MODULE
RCPP_MODULE(skmSolution_module) {

  class_<skmSolution>( "skmSolution" )

  .constructor<arma::uvec, arma::uvec, double>()

  .field( "s", &skmSolution::s )
  .field( "t", &skmSolution::t )
  .field( "o", &skmSolution::o )
  ;

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
skmSolution skm_minmax_cpp(const arma::mat x, const arma::uvec s_must) {

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

// [[Rcpp::export]]
skmSolution skm_cpp(const arma::mat x, const arma::uvec s_init,
                    const arma::uvec s_must, const arma::uword max_it = 2) {

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

    skmSolution xs = skm_minmax_cpp(gx, s_must);

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
