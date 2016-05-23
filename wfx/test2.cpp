#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

class Uniform {
  public:
    Uniform(double min_, double max_) : min(min_), max(max_) {}
  NumericVector draw(int n) const {
    RNGScope scope;
    return runif( n, min, max );
  }
  double min, max;
};

double uniformRange( Uniform* w) {
  return w->max - w->min;
}

RCPP_MODULE(unif_module) {
  class_<Uniform>( "Uniform" )
  .constructor<double,double>()
  .field( "min", &Uniform::min )
  .field( "max", &Uniform::max )
  .method( "draw", &Uniform::draw )
  .method( "range", &uniformRange )
  ;
}

class skmSolution {

public:

  //constructor
  skmSolution(arma::uvec s_, arma::uvec t_, double o_) : s(s_) , t(t_), o(o_) {}

  arma::uvec s; arma::uvec t; double o;
};

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
