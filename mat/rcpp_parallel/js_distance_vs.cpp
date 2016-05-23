#include <Rcpp.h>
using namespace Rcpp;

#include <cmath>
#include <algorithm>

// generic function for kl_divergence
template <typename InputIterator1, typename InputIterator2>
inline double kl_divergence(InputIterator1 begin1, InputIterator1 end1,
                            InputIterator2 begin2) {

  // value to return
  double rval = 0;

  // set iterators to beginning of ranges
  InputIterator1 it1 = begin1;
  InputIterator2 it2 = begin2;

  // for each input item
  while (it1 != end1) {

    // take the value and increment the iterator
    double d1 = *it1++;
    double d2 = *it2++;

    // accumulate if appropirate
    if (d1 > 0 && d2 > 0)
      rval += std::log(d1 / d2) * d1;
  }
  return rval;
}

// helper function for taking the average of two numbers
  inline double average(double val1, double val2) {
    return (val1 + val2) / 2;
  }

// [[Rcpp::export]]
NumericMatrix rcpp_js_distance(NumericMatrix mat) {

  // allocate the matrix we will return
  NumericMatrix rmat(mat.nrow(), mat.nrow());

  for (int i = 0; i < rmat.nrow(); i++) {
    for (int j = 0; j < i; j++) {

      // rows we will operate on
      NumericMatrix::Row row1 = mat.row(i);
      NumericMatrix::Row row2 = mat.row(j);

      // compute the average using std::tranform from the STL
      std::vector<double> avg(row1.size());
      std::transform(row1.begin(), row1.end(), // input range 1
                     row2.begin(),             // input range 2
                     avg.begin(),              // output range
                     average);                 // function to apply

      // calculate divergences
      double d1 = kl_divergence(row1.begin(), row1.end(), avg.begin());
      double d2 = kl_divergence(row2.begin(), row2.end(), avg.begin());

      // write to output matrix
      rmat(i,j) = std::sqrt(.5 * (d1 + d2));
    }
  }

  return rmat;
}
