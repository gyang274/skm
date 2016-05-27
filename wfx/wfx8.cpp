#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <RcppParallel.h>
// [[Rcpp::depends(RcppParallel, RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;
// using namespace RcppParallel;

//' stratified_sampling: from v select k w.r.t stratify structure g.
//' TODO: implementing via template so v is flexible as vec or uvec.
//' @param v: candidate v.
//' @param k: selection k.
//' @param g: stratify structure g.
//' @return s: select from v length k stratified by g.
// [[Rcpp::export]]
arma::uvec stratified_sampling(const arma::uvec& v, const arma::uword k, const arma::uvec& g) {

  Rcout << "stratified_sampling: check input - v: " << v.t() << std::endl;
  Rcout << "stratified_sampling: check input - k: " << k << std::endl;
  Rcout << "stratified_sampling: check input - g: " << g.t() << std::endl;

  // check k <= v.size()
  if ( k > v.size() ) { stop("stratified_sampling: k must <= v.size().\n"); }

  if ( g.size() != v.size() ) { stop("stratified_sampling: must have g.size() == v.size().\n"); }

  // special case k == 1 or k == v.size()
  if ( k == 1 ) { return RcppArmadillo::sample(v, 1, false); }

  if ( k == v.size() ) { return v; }

  // g_g: index of each stratification
  arma::uvec g_g = arma::unique(g);

  Rcout << "g_g: " << g_g.t() << std::endl;

  // n: number of stratification in total
  arma::uword n = g_g.size();

  Rcout << "n: " << n << std::endl;

  // special case g_g.size() == 1 no stratification
  if ( n == 1 ) { return RcppArmadillo::sample(v, k, false); }

  // quotient + remainder stratified sampling method
  // TODO: more complicate percentage of total style

  // v_g: v value belong to each group
  arma::field<arma::uvec> v_g(n);

  // k_g: select k_g from each group g with a total k
  arma::uvec k_g = arma::zeros<arma::uvec>(n);

  // r_g: number of remainder r w.r.t on each group g
  arma::uvec r_g(n);

  for ( arma::uword i = 0; i < n; i++) {

    v_g(i) = find(g == g_g(i));

    r_g(i) = v_g(i).size();

  }

  Rcout << "v_g: " << v_g << std::endl;
  Rcout << "k_g: " << k_g.t() << std::endl;
  Rcout << "r_g: " << r_g.t() << std::endl;

  // init q and r - q as a variable version of k
  arma::uword q(k);

  Rcout << "q: " << q << std::endl;

  arma::uvec q_g = arma::zeros<arma::uvec>(n);

  arma::uvec r_g_p = find(r_g > 0);

  Rcout << "r_g_p: " << r_g_p.t() << std::endl;

  // q / n - quotient
  while ( q / r_g_p.size() > 0 ) {

    q_g(r_g_p).fill(q / r_g_p.size());

    Rcout << "q_g: " << q_g.t() << std::endl;

    q = q % r_g_p.size();

    Rcout << "q: " << q << std::endl;

    for ( arma::uword i = 0; i < r_g_p.size(); i++ ) {

      if ( q_g(r_g_p(i)) > r_g(r_g_p(i)) ) {

        q += q_g(r_g_p(i)) - r_g(r_g_p(i));

        q_g(r_g_p(i)) = r_g(r_g_p(i));

        r_g(r_g_p(i)) = 0;

      } else {

        r_g(r_g_p(i)) = r_g(r_g_p(i)) - q_g(r_g_p(i));

      }

      k_g(r_g_p(i)) += q_g(r_g_p(i));

    }

    Rcout << "q_g: " << q_g.t() << std::endl;
    Rcout << "k_g: " << k_g.t() << std::endl;
    Rcout << "r_g: " << r_g.t() << std::endl;

    Rcout << "q: " << q << std::endl;

    r_g_p = find(r_g > 0);

    Rcout << "r_g_p: " << r_g_p.t() << std::endl;

    q_g.fill(0); // immaculatism and protection!

  }

  // q % n - remainder

  arma::uvec r_g_idx = RcppArmadillo::sample(r_g_p, q % r_g_p.size(), false);

  Rcout << "r_g_idx: " << r_g_idx.t() << std::endl;

  k_g(r_g_idx) += 1;

  Rcout << "k_g: " << k_g.t() << std::endl;

  // select k_g from each v_g into s
  arma::uvec s;

  for (arma::uword i = 0; i < n; i++) {

    s = arma::join_cols(s, RcppArmadillo::sample(v_g(i), k_g(i), false));

  }

  Rcout << "stratified_sampling: construct output - s: " << s.t() << std::endl;

  return s;

}
