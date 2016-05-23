// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppParallel.h>


using namespace Rcpp;
using namespace RcppParallel;


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


struct JsDistance : public Worker
{
  const RMatrix<double> tmp_MAT;  // input matrix to read from
  RMatrix<double> tmp_rmat;       // output matrix to write to
  std::size_t row_size, col_size;
  
  // Convert global input/output into RMatrix/RVector type
  JsDistance(const NumericMatrix& matrix_input, NumericMatrix& matrix_output, 
             std::size_t row_size, std::size_t col_size)
    : tmp_MAT(matrix_input), tmp_rmat(matrix_output), row_size(row_size), col_size(col_size) {}
  
  // convert RVector/RMatrix into arma type for Rcpp function
  // and the follwing arma data will be shared in parallel computing
  arma::mat convert()
  {
    RMatrix<double> tmp_mat = tmp_MAT;
    arma::mat MAT(tmp_mat.begin(), row_size, col_size, false);
    return MAT;
  }
  
  
  void operator()(std::size_t begin, std::size_t end)
  {
    for (std::size_t i = begin; i < end; i++)
    {
      for (std::size_t j = 0; j < i; j++)
      {
        // rows we will operate on
        arma::mat MAT = convert();
        arma::rowvec row1 = MAT.row(i);          // get the row of arma matrix
        arma::rowvec row2 = MAT.row(j);
        
        // compute the average using std::tranform from the STL
        std::vector<double> avg(row1.n_elem);
        std::transform(row1.begin(), row1.end(), // input range 1
                       row2.begin(),             // input range 2
                       avg.begin(),              // output range
                       average);                 // function to apply
        
        // calculate divergences
        double d1 = kl_divergence(row1.begin(), row1.end(), avg.begin());
        double d2 = kl_divergence(row2.begin(), row2.end(), avg.begin());
        
        // write to output matrix
        tmp_rmat(i,j) = sqrt(.5 * (d1 + d2));
      }
    }
  }
};

// [[Rcpp::export]]
NumericMatrix rcpp_parallel_js_distance_modify(const Rcpp::NumericMatrix& matrix_input, int N_cores)
{
  // allocate the matrix we will return
  NumericMatrix matrix_output(matrix_input.nrow(), matrix_input.nrow());
  std::size_t row_size = matrix_input.nrow();
  std::size_t col_size = matrix_input.ncol();
  
  // create the worker
  JsDistance jsDistance(matrix_input, matrix_output, row_size, col_size);
  
  // call it with parallelFor
  parallelFor(0, matrix_input.nrow(), jsDistance, matrix_input.nrow()/N_cores);           // parallelFor with grain size setting
  
  return matrix_output;
}
