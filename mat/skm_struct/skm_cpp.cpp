#include "skm.h"
#include "matrix_minmax.h"

#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]


// http://stackoverflow.com/questions/21944695/rcpparmadillo-and-arma-namespace

// cann't use <using namespace arma> in developing r-package with RcppArmadillo
// because Rcpp when it compiles C++ source files RcppExports.cpp does not copy
// over the using namespace arma statements into there.

// since different files can use different namespace, so Rcpp attributes parser
// can't just copy all the using namespace ... statements into RcppExports.cpp,
// so it just defaults to using the Rcpp namespace. If Rcpp just copied all the
// using namespace statements willy nilly into RcppExports.cpp surely conflict.

// fix is either to explicitly prefix arma:: or modify the RcppExports.cpp file
// and add using namespace arma; at the top - remember you'd have to do it each
// time after calling compileAttributes(). - Kevin Ushey

using namespace Rcpp;
// using namespace arma;
using namespace RcppParallel;


// skm_minmax_cpp: skm via min-max on in cpp - subroutine of skm_sgl_cpp calls
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
// [[Rcpp::export]]
arma::vec skm_minmax_cpp(const arma::mat& x, const arma::uvec s_must) {

  // Rcout << "skm_minmax_cpp - check input x: " << std::endl << x << std::endl;
  // Rcout << "skm_minmax_cpp - check input s_must: " << s_must.t() << std::endl;

  arma::vec skmSolution(x.n_cols + 1); skmSolution.fill(-1);

  Rcout << "skm_minmax_cpp - init skmSolution: " << skmSolution << std::endl;

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
      // contains one or more min_val, we should remove some one column
      // loss of val(on row s) - min because we don't want then selects
      // another row s2 that is very close to s - e.g. s has 2nd min on
      // col t but s2 has 1st min on col t is then select - a real case
      // example would be we have two group from t, says, East and West
      // we put NJ 2nd best among East in s_must and then select NY 1st
      // instead we should remove East from consideration and select CA
      // when remove col w.r.t lowest val(on row s) - min, it mean that
      // on this column or group, s is closest and replaces best option

      // Rcout << "push s_must into s: " << s_must(i_init) << std::endl;

      // arma::uvec v_min_idx = arma::zeros<arma::uvec>(x.n_cols);

      // calculate colwise val(on row s) - min: what are the cost of enforcing s
      arma::vec v_cns_val = arma::zeros<arma::vec>(x.n_cols);

      for (arma::uvec::iterator jt = vlmt.begin(); jt != vlmt.end(); jt++) {

        v_cns_val(*jt) = x(s(i_init), *jt) - col_min_val(x.col(*jt), ulmt);

      }

      // TODO: add when row s contains more than one columns achieves:
      // col_min_val(v_cns_val, vlmt) select the one with max(max-min)
      vlmt = vlmt(find(vlmt != col_min_idx(v_cns_val, vlmt)));

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

  skmSolution(0) = 0;

  for (arma::uword i = 0; i < x.n_cols; i++) {

    skmSolution(0) += x(s(i), t(i));

    skmSolution(i + 1) = s(i);

  }

  Rcout << "skm_minmax_cpp - ende skmSolution: " << skmSolution << std::endl;

  return skmSolution;
}


// Rcpp attributes code that parses function declarations isn't able to parsing
// all syntactic forms of C++ but rather a subset. The default argument parsing
// is able to handle scalars, strings, and simple vector initializations but no
// more complex expressions like ucolvec() - J.J.Allaire <jj.allaire@gmail.com>

// workaourd? call function with argu v = ucolvec() and test with v.size() == 0

// skm_sgl_cpp: solve skm with single and a fixed given s_init
// member function of class skm: w. mat x, uvec s_init, uvec s_must, int max_it
arma::vec skm_sgl_cpp(const arma::mat& x, arma::uvec s_init, arma::uvec s_must, arma::uword max_it) {

  // Rcout << "skm_sgl_cpp - check input x: " << std::endl << x << std::endl;
  Rcout << "skm_sgl_cpp - check input s_init: " << s_init.t() << std::endl;
  Rcout << "skm_sgl_cpp - check input s_must: " << s_must.t() << std::endl;
  Rcout << "skm_sgl_cpp - check input max_it: " << max_it << std::endl;

  arma::vec skmSolution(s_init.size() + 1); skmSolution.fill(-1);

  Rcout << "skm_sgl_cpp - init skmSolution: " << skmSolution << std::endl;

  arma::uvec s(s_init.begin(), s_init.size(), false);

  arma::uvec t(x.n_cols);

  double o = std::numeric_limits<double>::max();

  // Rcout << "s : " << std::endl << s << std::endl;

  // Rcout << "t : " << std::endl << t.t() << std::endl;

  // init on while loop check
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
    arma::mat gx(x.n_rows, s_init.size());

    for (arma::uword i = 0; i < s.size(); i++) {

      arma::uvec g = find(t == s(i));

      // Rcout << "g : " << g << std::endl;

      // sum(matrix, 0) row_wise_sum
      // sum(matrix, 1) col_wise_sum

      if ( g.size() > 0 ) {

        gx.col(i) = sum(x.cols(g), 1);

      } else {

        Rcout << "skm_sgl_cpp - edge case: some s is subordinate to combining other s ... \n" << std::endl;

        gx.col(i) = sum(x, 1) / (s.size() * 10.0);

      }

    }

    // Rcout << "gx :" << gx << std::endl;

    skmSolution = skm_minmax_cpp(gx, s_must);

    // Rcout << "skmSolution : " << skmSolution << std::endl;

    if ( !(skmSolution(0) < o) ) { archive_optim = true; }

    o = skmSolution(0);

    for (arma::uword i = 0; i < skmSolution.size() - 1; i++) {

      s(i) = skmSolution(i + 1);

    }

    num_it++;

  }

  // Rcout << "construct solution: " << std::endl;

  // s and o are set, only need to update t
  // for (arma::uword j = 0; j < x.n_cols; j++) {
  //
  //   t(j) = col_min_idx(x.col(j), s);
  //
  // }

  // Rcout << "s: " << s << std::endl;

  // Rcout << "t: " << t.t() << std::endl;

  // Rcout << "o: " << o << std::endl;

  Rcout << "skm_sgl_cpp - ende skmSolution: " << skmSolution << std::endl;

  return skmSolution;
}


// skm_rgi_cpp: solve skm with single and random size k s_init
// member function of class skm: w. mat x, uword k, uvec s_must, int max_it
arma::vec skm_rgi_cpp(const arma::mat& x, arma::uword k, arma::uvec s_must, arma::uword max_it) {

  // Rcout << "skm_rgi_cpp - check input x: " << std::endl << x << std::endl;

  // create s_init w s_must and k
  arma::uvec ulmt = arma::cumsum(arma::ones<arma::uvec>(x.n_rows)) - 1;

  if ( s_must.size() > 0 ) {

    if ( s_must.size() < k ) {

      for (arma::uword i = 0; i < s_must.size(); i++) {

        ulmt = ulmt(find(ulmt != s_must(i)));

      }

    } else {

      stop("skm_rgi_cpp: s_must must have length < k");

    }

  }

  arma::uvec s_init = arma::join_cols(s_must, as<arma::uvec>(RcppArmadillo::sample(as<IntegerVector>(wrap(ulmt)), as<int>(wrap(k - s_must.size())), false)));

  // Rcout << "skm_rgi_cpp - construct s_init: " << s_init.t() << std::endl;

  return skm_sgl_cpp(x, s_init, s_must, max_it);

}


// skm_rgi_rpl: a wrapper around skm_rgi_cpp for calling from skmRpl struct
// skm_rgi_rpl(x.begin(), x.nrow(), x.ncol(), arg.begin(), os.row(i).begin());
void skm_rgi_rpl(const arma::mat& x,
                 RcppParallel::RVector<int>::const_iterator it_arg_begin,
                 RcppParallel::RMatrix<double>::Row::iterator it_os_begin) {

  // Rcout << "skm_rgi_rpl - call enter point ..." << std::endl;

  // construct argument list for calling skm_rgi_cpp

  // Rcout << "skm_rgi_rpl - construct arg list x: " << std::endl << x << std::endl;

  RVector<int>::const_iterator it1 = it_arg_begin;

  RMatrix<double>::Row::iterator it2 = it_os_begin;

  // take the value and increment the iterator
  arma::uword k(*it1); it1++;

  arma::uword max_it(*it1); it1++;

  arma::uvec  s_must(*it1);

  if ( s_must.size() > 0 ) {

    for (arma::uword i = 0; i < s_must.size(); i++) {

      it1++; s_must(i) = *it1;

    }

  }

  // Rcout << "skm_rgi_rpl - construct arg list k: " << k << std::endl;
  // Rcout << "skm_rgi_rpl - construct arg list max_it: " << max_it << std::endl;
  // Rcout << "skm_rgi_rpl - construct arg list s_must: " << s_must.t() << std::endl;

  arma::vec skmSolution = skm_rgi_cpp(x, k, s_must, max_it);

  // construct os - <o, s>

  for (unsigned int i = 0; i < skmSolution.size() ; i++) {

    *it2 = skmSolution(i); it2++;

  }

  // Rcout << "skm_rgi_rpl - construct skmRplSolution: " << skmSolution << std::endl;

}


// skmRpl - RcppParallel

struct skmRpl : public Worker {

  // x - input matrix
  // s<source> in row
  // t<target> in col
  // d<distance> cell
  const RMatrix<double> x;

  // s - return optim s of each single run
  // RMatrix<int> s;

  // t - return group of t w.r.t optimum s
  // RMatrix<int> t;

  // o - return objective o w.r.t optimum s and t
  // RVector<double> o;

  // call skm_rgi_rpl with return vector os<o, s>
  RMatrix<double> os;

  // must use RMatrix and RVector to communicate with RcppParallel
  // gc() issue would happen if not and cause R crashes very badly
  const RVector<int> arg; // with k, max_it, length_of_s_must and s_must

  // k - option number of s in optimum s, k <1..x.n_rows>
  // const arma::uword k;

  // s_must - option s_must list of s must in (optim) s
  // const arma::uvec s_must;

  // max_at - option max_at number of runs in total
  // const arma::uword max_at;

  // max_it - option max_it number of iter each run
  // const arma::uword max_it;

  // .constructor convert input/output into RMatrix/RVector type for RcppParallel
  skmRpl(const NumericMatrix& x, NumericMatrix& os, const IntegerVector& arg) : x(x), os(os), arg(arg) {}

  // create a converter convert RMatrix/RVector into arma for RcppArmadillo calls
  arma::mat skmRplConvertX() {

    RMatrix<double> _x = x;

    arma::mat a_x(_x.begin(), _x.nrow(), _x.ncol(), false);

    return a_x;

  }

  // parallel calls to skm_rgi_cpp
  void operator()(std::size_t begin, std::size_t end) {

    // -> going to be a ParallelFor!
    for (std::size_t i = begin; i < end; i++) {

      // check for user interrupts
      // Rcpp::checkUserInterrupt();

      skm_rgi_rpl(skmRplConvertX(), arg.begin(), os.row(i).begin());

    }

  }

};

// skm_mlp_cpp: solve skm with multiple runs in parallel
// RcppParallel <http://rcppcore.github.io/RcppParallel>

// [[Rcpp::export]]
List skm_mlp_cpp(const NumericMatrix x, const unsigned int k, const IntegerVector s_must,
                 const unsigned int max_it, const unsigned int max_at) {

  NumericMatrix os(max_at, k + 1);

  // Rcout << "skm_mlp_cpp - check input x: " << std::endl << x << std::endl;
  // Rcout << "skm_mlp_cpp - check input os: " << os << std::endl;
  // Rcout << "skm_mlp_cpp - check input k: " << k << std::endl;
  // Rcout << "skm_mlp_cpp - check input s_must: " << s_must << std::endl;
  // Rcout << "skm_mlp_cpp - check input max_it: " << max_it << std::endl;
  // Rcout << "skm_mlp_cpp - check input max_at: " << max_at << std::endl;

  IntegerVector arg(3 + s_must.size());

  arg(0) = k;

  arg(1) = max_it;

  arg(2) = s_must.size();

  if ( s_must.size() > 0 ) {

    for (unsigned int i = 0; i < s_must.size(); i++) {

      arg(3 + i) = s_must(i);

    }

  }

  // Rcout << "skm_mlp_cpp - construct arg: " << arg << std::endl;

  skmRpl a_skmRpl(x, os, arg);

  parallelFor(0, max_at, a_skmRpl);

  return Rcpp::List::create(Rcpp::Named("os") = os);

}



