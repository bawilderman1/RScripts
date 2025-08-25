#include "ultimate_smoother.h"
#include <cmath>
#include <vector>

// [[Rcpp::export]]
Rcpp::NumericVector ultimateSmoother(const Rcpp::NumericVector& price, int period, int displace) {
    int n = price.size();
    std::vector<double> us(n, 0.0);
    Rcpp::NumericVector result(n);

    double a1 = std::exp(-1.414 * M_PI / period);
    double b1 = 2 * a1 * std::cos(1.414 * M_PI / period);
    double c2 = b1;
    double c3 = -a1 * a1;
    double c1 = (1 + c2 - c3) / 4;

    for (int i = 2; i < n; ++i) {
        us[i] = (1 - c1) * price[i] +
                (2 * c1 - c2) * price[i - 1] -
                (c1 + c3) * price[i - 2] +
                c2 * us[i - 1] +
                c3 * us[i - 2];
    }

    for (int i = 0; i < n; ++i) {
        if (i >= displace) {
            result[i] = std::round(us[i - displace] * 100.0) / 100.0;
        } else {
            result[i] = R_NaN;
        }
    }

    return result;
}

// [[Rcpp::export]]
Rcpp::DataFrame ultimateSmootherTbl(const Rcpp::NumericVector& price, int period, int displace) {
    Rcpp::NumericVector smoother_values = ultimateSmoother(price, period, displace);

    return Rcpp::DataFrame::create(
        Rcpp::Named("UltimateSmoother") = smoother_values
    );
}


// [[Rcpp::export]]
Rcpp::NumericVector calculateSuperSmoother(const Rcpp::NumericVector& price, int period) {
    int n = price.size();
    Rcpp::NumericVector result(n);

    double a1 = std::exp(-1.414 * M_PI / (0.5 * period));
    double b1 = 2 * a1 * std::cos(1.414 * M_PI / (0.5 * period));
    double c2 = b1;
    double c3 = -a1 * a1;
    double c1 = 1 - c2 - c3;

    for (int i = 2; i < n; ++i) {
        result[i] = c1 * (price[i] + price[i - 1]) / 2 + c2 * result[i - 1] + c3 * result[i - 2];
    }

    return result;
}