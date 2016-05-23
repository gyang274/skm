#ifndef __SKM__
#define __SKM__


#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;


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

class skmSolution;

class skm {

public:

  skm(arma::mat x_) : x(x_) {}

  arma::mat x;

  skmSolution skm_cpp(arma::uvec s_init, arma::uvec s_must, arma::uword max_it);

  skmSolution skm_mlp(arma::uvec s_init, arma::uvec s_must, arma::uword max_it, arma::uword max_at);

private:

  skmSolution skm_minmax_cpp(const arma::mat x, const arma::uvec s_must);

};

class skmSolution {

public:

  //constructor
  skmSolution(arma::uvec s_, arma::uvec t_, double o_) : s(s_) , t(t_), o(o_) {}

  arma::uvec s; arma::uvec t; double o;
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

    .method("skm_cpp", &skm::skm_cpp)

    .method("skm_mlp", &skm::skm_mlp)
    ;

  class_<skmSolution>( "skmSolution" )

    .constructor<arma::uvec, arma::uvec, double>()

    .field( "s", &skmSolution::s )
    .field( "t", &skmSolution::t )
    .field( "o", &skmSolution::o )
    ;

}


#endif // __SKM__
