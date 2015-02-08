#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::depends("BH")]]

#include <boost/multiprecision/cpp_dec_float.hpp>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/random.hpp>
#include <boost/random/gamma_distribution.hpp>
#include <boost/math/distributions.hpp>

#include <numeric>
#include <algorithm>
#include <map>
#include <string>
#include <iostream>

using namespace Rcpp;
using namespace arma;
using namespace std;
using namespace boost::multiprecision;
using namespace boost::math;

boost::mt19937 rng; // seed for random sampling

double rbeta_cpp(double shape1, double shape2) {
  double u  = rand() / double(RAND_MAX);
  beta_distribution<> beta_dist(shape1, shape2);
  return quantile(beta_dist, u);  
}

// [[Rcpp::export]]
arma::rowvec rDirichlet2(arma::rowvec param, int length) {
  vector<double> ret;
  param *= 10000;
  vector<double> param_vec = conv_to<vector<double> >::from(param);
  int len = length - 1;
  
  double paramSum = std::accumulate(param_vec.begin()+1,param_vec.end(),(double)0);
  ret.push_back(rbeta_cpp(param_vec[0], paramSum));
  for (int i=1; i<len;i++) {
    double paramSum = std::accumulate(param_vec.begin()+i+1,param_vec.end(),(double)0); 
    double phi = rbeta_cpp(param_vec[i], paramSum);
    double sumRet = std::accumulate(ret.begin(),ret.end(),(double)0);  
    ret.push_back((1-sumRet) * phi);
  }   
  double sumRet = std::accumulate(ret.begin(),ret.end(),(double)0); 
  ret.push_back(1-sumRet);
  return ret;
}  

