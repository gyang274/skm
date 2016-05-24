// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>


using namespace Rcpp;
using namespace RcppParallel;


class skm;

class skmSolution;

class skm {

public:

  skm(arma::mat x_): x(x_) {}

  arma::mat x;

  skmSolution skm_sgl_cpp(arma::uword i);
};

class skmSolution {

public:

  //constructor
  skmSolution(arma::uvec s_, arma::uvec t_, double o_) : s(s_) , t(t_), o(o_) {}

  arma::uvec s; arma::uvec t; double o;
};

skmSolution skm::skm_sgl_cpp(arma::uword i) {

  arma::uvec s(2); s.fill(i);

  arma::uvec t(3); t.fill(i);

  double o; o = std::sqrt(i);

  return skmSolution(s, t, o);
}


class skmRplTest : public Worker {

private:

  const RMatrix<double> x;

  RMatrix<int> s;

  RMatrix<int> t;

  RVector<double> o;

public:

  // .constructor can only take input/output via RMatrix/RVector Type
  skmRplTest(const NumericMatrix& x_, IntegerMatrix s_, IntegerMatrix t_, NumericVector& o_) : x(x_), s(s_), t(t_), o(o_) {}

  // convert RMatrix/RVector into arma for user defined Rcpp function
  arma::mat xkvt() {

    RMatrix<double> _x = x;

    arma::mat a_x(_x.begin(), _x.nrow(), _x.ncol(), false);

    return a_x;

  }

  void operator()(std::size_t begin, std::size_t end) {

    for (std::size_t i = begin; i < end; i++) {

      skm a_skm(xkvt());

      skmSolution a_skmSolution = a_skm.skm_sgl_cpp(i);

      o[i] = a_skmSolution.o;

      for (arma::uword j = 0; j < a_skmSolution.s.size(); j++) {

        s(i, j) = a_skmSolution.s(j);

      }

      for (arma::uword j = 0; j < a_skmSolution.t.size(); j++) {

        t(j, i) = a_skmSolution.t(j);

      }

    }
  }

};

// [[Rcpp::export]]
int skmRplTestFunc(const NumericMatrix& x, const int k) {

  NumericVector o(k);

  IntegerMatrix s(k, k);

  IntegerMatrix t(k, k);

  Rcout << "init s: " << std::endl << s << std::endl;

  Rcout << "init t: " << std::endl << t << std::endl;

  skmRplTest a_skmRplTest(x, s, t, o);

  parallelFor(0, k, a_skmRplTest);

  Rcout << "aftr s: " << std::endl << s << std::endl;

  Rcout << "aftr t: " << std::endl << t << std::endl;

  return 42;
}


