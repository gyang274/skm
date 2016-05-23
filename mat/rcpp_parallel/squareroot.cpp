// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>


// <http://rcppcore.github.io/RcppParallel/>

// install.packages("RcppParallel")

// code written within parallel workers should never call R or Rcpp API at all.
// bcoz R is single-threaded and concurrent interaction with its data structure
// can cause crashes and other undefined behavior. - Dirk Eddelbuettel

using namespace Rcpp;
using namespace RcppParallel;

struct SquareRoot : public Worker
{
  // source matrix
  const RMatrix<double> input;

  // destination matrix
  RMatrix<double> output;

  // initialize with source and destination
  SquareRoot(const Rcpp::NumericMatrix input, Rcpp::NumericMatrix output) : input(input), output(output) {}

  // take the square root of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin,
                   input.begin() + end,
                   output.begin() + begin,
                   std::sqrt);
  }
};

// [[Rcpp::export]]
Rcpp::NumericMatrix parallelMatrixSqrt(Rcpp::NumericMatrix x) {

  // allocate the output matrix
  Rcpp::NumericMatrix output(x.nrow(), x.ncol());

  // SquareRoot functor (pass input and output matrixes)
  SquareRoot squareRoot(x, output);

  // call parallelFor to do the work
  parallelFor(0, x.length(), squareRoot);

  // return the output matrix
  return output;
}
