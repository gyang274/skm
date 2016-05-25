#ifndef __SKM__
#define __SKM__


#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;
using namespace RcppParallel;


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

class skm;

class skmRpl;

class skmSolution;

class skm {

public:

  arma::mat x;

  skm(arma::mat x) : x(x) {}

  // skm::skm_sgl_cpp: solve skm with single and a fixed given s_init
  skmSolution skm_sgl_cpp(arma::uvec s_init, arma::uvec s_must, arma::uword max_it);

  // skm::skm_rgi_cpp: solve skm with single and random size k s_init
  skmSolution skm_rgi_cpp(arma::uword k, arma::uvec s_must, arma::uword max_it);

  // skm::skm_rgi_rpl: a wrapper around skm::skm_rgi_cpp for calling from skmRpl class
  void skm_rgi_rpl(RcppParallel::RVector<int>::const_iterator arg_begin,
                   RcppParallel::RVector<int>::const_iterator arg_end,
                   RcppParallel::RMatrix<double>::Row::iterator os_begin);

private:

  skmSolution skm_minmax_cpp(const arma::mat x, const arma::uvec s_must);

};

class skmSolution {

public:

  arma::uvec s; arma::uvec t; double o;

  // .constructor
  skmSolution(arma::uvec s_, arma::uvec t_, double o_) : s(s_) , t(t_), o(o_) {}

};

class skmRpl : public Worker {

private:

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

  // call skm_rgi_rpl with return RVector<int> of <o, s>
  RMatrix<double> os;

  // k - option number of s in optimum s, k <1..x.n_rows>
  // const arma::uword k;

  // s_must - option s_must list of s must in (optim) s
  // const arma::uvec s_must;

  // max_at - option max_at number of runs in total
  // const arma::uword max_at;

  // max_it - option max_it number of iter each run
  // const arma::uword max_it;

  // must use RMatrix and RVector to communicate with RcppParallel
  // gc() issue would happen if not and cause R crashes very badly
  const RVector<int> arg; // with k, max_it, length_of_s_must and s_must

public:

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

      // construct a skm object
      skm a_skm(skmRplConvertX());

      // skm::skm_rgi_cpp(arma::uword k, arma::uvec s_must, arma::uword max_it);
      a_skm.skm_rgi_rpl(arg.begin(), arg.end(), os.row(i).begin());

      // Rcout << "skmRpl - check output at i: " << skmRplSolution << std::endl;

    }

  }

};

// <http://dirk.eddelbuettel.com/code/rcpp/Rcpp-modules.pdf>
// expose class into R using RCPP_MODULE

RCPP_EXPOSED_CLASS(skm);

RCPP_EXPOSED_CLASS(skmSolution);

RCPP_MODULE(skm_module) {

  using namespace Rcpp;

  class_<skm>( "skm" )

    .constructor<arma::mat>()

    .field("x", &skm::x)

    .method("skm_sgl_cpp", &skm::skm_sgl_cpp)

    .method("skm_rgi_cpp", &skm::skm_rgi_cpp)
    ;

  class_<skmSolution>( "skmSolution" )

    .constructor<arma::uvec, arma::uvec, double>()

    .field( "s", &skmSolution::s )
    .field( "t", &skmSolution::t )
    .field( "o", &skmSolution::o )
    ;

}


#endif // __SKM__
