#include <Rcpp.h>
using namespace Rcpp;


List brute_force_cpp(NumericVector w, NumericVector v, double W) {
  int n = w.size();
  int max_val = 0;
  IntegerVector best_elements;

  int num_comb = 1 << n;
  for (int i = 0; i < num_comb; ++i) {
    double current_w = 0;
    double current_v = 0;
    IntegerVector current_elements;

    for (int j = 0; j < n; ++j) {
      if ((i >> j) & 1) {
        current_w += w[j];
        current_v += v[j];
        current_elements.push_back(j + 1);
      }
    }

    if (current_w <= W && current_v > max_val) {
      max_val = current_v;
      best_elements = current_elements;
    }
  }

  return List::create(_["value"] = max_val, _["elements"] = best_elements);
}
