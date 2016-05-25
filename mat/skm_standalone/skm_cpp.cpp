#include <Rcpp.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel)]]

using namespace Rcpp;
using namespace RcppParallel;

void skm_rgi_rpl(RMatrix<double>::const_iterator it_x_begin,
                 RVector<int>::const_iterator it_arg_begin,
                 RMatrix<double>::Row::iterator it_os_begin) {

  Rcout << "skm_rgi_rpl - call enter point ..." << std::endl;

  // Rcout << "skm_rgi_rpl - construct argument list x: " << std::endl << x << std::endl;

  RVector<int>::const_iterator it1 = it_arg_begin;

  RMatrix<double>::Row::iterator it2 = it_os_begin;

  // construct argument list for calling skm_rgi_cpp
  int k(*it1); it1++;

  int max_it(*it1); it1++;

  IntegerVector  s_must(*it1);

  if ( s_must.size() > 0 ) {

    for (int i = 0; i < s_must.size(); i++) {

      it1++; s_must(i) = *it1;

    }

  }

  Rcout << "skm_rgi_rpl - construct argument list k: " << k << std::endl;
  Rcout << "skm_rgi_rpl - construct argument list max_it: " << max_it << std::endl;
  Rcout << "skm_rgi_rpl - construct argument list s_must: " << s_must << std::endl;

  // arma::vec skmSolution = skm_rgi_cpp(x, k, s_must, max_it);

  // construct os - <o, s>

  // for (unsigned int i = 0; i < skmSolution.size() ; i++) {
  //
  //   *it2 = skmSolution(i); it2++;
  //
  // }

  for (unsigned int i = 0; i < 3 ; i++) {

    *it2 = 10.87; it2++;

  }

  // Rcout << "skm_rgi_rpl - construct skmRplSolution: " << skmSolution << std::endl;

}

// skmRpl - RcppParallel

struct skmRpl : public Worker {

  // x - input matrix with s <source> in row, t <target> in col and d <distance> cell
  const RMatrix<double> x;

  // os - output matrix with vector <o - objective w.r.t optim s - source on each run>
  RMatrix<double> os;

  // arg - argument list with k, max_it, length_of_s_must and s_must with index from 0
  const RVector<int> arg;

  // .constructor convert input/output into RMatrix/RVector type for RcppParallel
  skmRpl(const NumericMatrix& x, NumericMatrix& os, const IntegerVector& arg) : x(x), os(os), arg(arg) {}

  // parallel calls to skm_rgi_rpl
  void operator()(std::size_t begin, std::size_t end) {

    // -> going to be a ParallelFor!
    for (std::size_t i = begin; i < end; i++) {

      // check for user interrupts
      // Rcpp::checkUserInterrupt();

      Rcout << "skmRpl - schedule i: " << i << std::endl;

      skm_rgi_rpl(x.begin(), arg.begin(), os.row(i).begin());

    }

  }

};

// skm_mlp_cpp: solve skm with multiple runs in parallel

// [[Rcpp::export]]
List skm_mlp_cpp(const NumericMatrix x, const unsigned int k, const IntegerVector s_must,
                 const unsigned int max_it, const unsigned int max_at) {

  // Rcout << "skm_mlp_cpp - check input x: " << std::endl << x << std::endl;
  // Rcout << "skm_mlp_cpp - check input k: " << k << std::endl;
  // Rcout << "skm_mlp_cpp - check input s_must: " << s_must << std::endl;
  // Rcout << "skm_mlp_cpp - check input max_it: " << max_it << std::endl;
  // Rcout << "skm_mlp_cpp - check input max_at: " << max_at << std::endl;

  // construct output matrix
  NumericMatrix os(max_at, k + 1);

  // Rcout << "skm_mlp_cpp - construct output matrix os: " << os << std::endl;

  // construct argument list
  IntegerVector arg(3 + s_must.size());

  arg(0) = k;

  arg(1) = max_it;

  arg(2) = s_must.size();

  if ( s_must.size() > 0 ) {

    for (unsigned int i = 0; i < s_must.size(); i++) {

      arg(3 + i) = s_must(i);

    }

  }

  // Rcout << "skm_mlp_cpp - construct argument list arg: " << arg << std::endl;

  // create skmRpl object on parallel running
  skmRpl a_skmRpl(x, os, arg);

  parallelFor(0, max_at, a_skmRpl, 40);

  return Rcpp::List::create(Rcpp::Named("os") = os);

}
