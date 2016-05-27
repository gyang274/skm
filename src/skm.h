#ifndef __SKM__
#define __SKM__


#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;
// using namespace RcppParallel;


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

  double o; arma::uvec s; arma::vec o_list; arma::umat s_list;

  // .constructor
  skmSolution(double o, arma::uvec s) : o(o) , s(s), o_list(o), s_list(s) {}

};

RCPP_EXPOSED_CLASS(skmSolution);


skmSolution skm_minmax_cpp(const arma::mat& x, const arma::uvec& s_must);

skmSolution skm_sgl_cpp(const arma::mat& x, const arma::uvec s_init,
                        const arma::uvec& s_must, const arma::uword max_it);

skmSolution skm_rgi_cpp(const arma::mat& x, const arma::uword k,
                        const arma::uvec& s_must, const arma::uword max_it);

skmSolution skm_rgs_cpp(const arma::mat& x, const arma::uword k, const arma::uvec g,
                        const arma::uvec& s_must, const arma::uword max_it);


RCPP_MODULE(skm_module) {

  using namespace Rcpp;

  class_<skmSolution>( "skmSolution" )

    .constructor<double, arma::uvec>()

    .field( "o", &skmSolution::o )
    .field( "s", &skmSolution::s )
    ;

  function("skm_minmax_cpp", &skm_minmax_cpp,
           "skmSolution skm_minmax_cpp(const arma::mat& x, const arma::uvec& s_must)");

  function("skm_sgl_cpp", &skm_sgl_cpp,
           "skmSolution skm_sgl_cpp(const arma::mat& x, const arma::uvec s_init, const arma::uvec& s_must, const arma::uword max_it)");

  function("skm_rgi_cpp", &skm_rgi_cpp,
           "skmSolution skm_rgi_cpp(const arma::mat& x, const arma::uword k, const arma::uvec& s_must, const arma::uword max_it)");

  function("skm_rgs_cpp", &skm_rgs_cpp,
           "skmSolution skm_rgs_cpp(const arma::mat& x, const arma::uword k, const arma::uvec g, const arma::uvec& s_must, const arma::uword max_it)");

}


#endif // __SKM__
