// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>


using namespace std;
using namespace Rcpp;
using namespace RcppParallel;

struct MyThing : public Worker {
  RVector<double> _pc;
  RVector<double> _pcsd;
  
  MyThing(Rcpp::NumericVector _pc, Rcpp::NumericVector _pcsd) : _pc(_pc), _pcsd(_pcsd){}
  
  void operator()(std::size_t begin, std::size_t end) {
    
    for(int j = begin; j < end; j++) {      
      _pc[j] = 1;
      _pcsd[j] = 1;
    }
  }    
};

// [[Rcpp::export]]
void calculateMyThingParallel() {
  
  NumericVector _pc(100);
  NumericVector _pcsd(100);
  
  MyThing mt(_pc, _pcsd);
  
  parallelFor(0, 100, mt);
}

// [[Rcpp::export]]
NumericVector csample_num( NumericVector x,
                           int size,
                           bool replace,
                           NumericVector prob = NumericVector::create()
) {
  NumericVector ret = RcppArmadillo::sample(x, size, replace, prob);
  return ret;
}

