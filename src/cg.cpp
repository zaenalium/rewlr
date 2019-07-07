#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

double norm_mat(arma::mat A){
  return as_scalar(A.t() * A);
}

// [[Rcpp::export]]

arma::mat cg_cpp(arma::mat A, arma::mat b, arma::mat B) {
  arma::mat r = b - A * B;
  arma::mat d = r * 2;
  int ci = 1;
  while( (norm_mat(r) > 0.0005) && (ci < 200) )  {
    double s = as_scalar(r.t() * r) /as_scalar(d.t() * A * d);
    B = B +  s * d;
    arma::mat r1 = r - s * (A * d);
    double zeta  = as_scalar(r1.t() * r1) / as_scalar(r.t() * r);
    arma::mat d1 = r1 + (zeta * d);
    r = r1 * 1;
    d = d1 * 1;
    ci = ci + 1;
    }
  return B;}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/***R
A = matrix(c(62.0967159965735,309.446464646465,
           198.721212121212,115.220202020202,25.4161616161616,309.446464646465,
           1618.11166549152,1027.46565656566,620.545151515152,141.078888888889,
           198.721212121212,1027.46565656566,673.544594784453,368.433333333333,
           79.3007070707071,115.220202020202,620.545151515151,368.433333333333,
           291.664594784452,74.7042424242425,25.4161616161616,141.078888888889,
           79.3007070707071,74.7042424242425,23.521564481422), 5, 5)
b = matrix(c(-80.2020202020202,-382.307070707071,-288.157575757576,
             -61.959595959596,1.63232323232324), 5, 1)
B = matrix(0, 5, 1)

cg_cpp(A, b, B)

*/

