#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;


class skmSolution {

public:

  double o; arma::uvec s;

  // .constructor
  skmSolution(double o, arma::uvec s) : o(o) , s(s) {}

};

RCPP_EXPOSED_CLASS(skmSolution);

RCPP_MODULE(skm_module) {

  using namespace Rcpp;

  class_<skmSolution>( "skmSolution" )

    .constructor<double, arma::uvec>()

    .field( "o", &skmSolution::o )
    .field( "s", &skmSolution::s )
    ;

}

// [[Rcpp::export]]
skmSolution createSkmSolution(double o, arma::uvec& s) {
  
  return skmSolution(o, s);

}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
a = createSkmSolution(o = 10.87, s = c(1:10))
*/
