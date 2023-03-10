// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// observation_impute_cpp_simple
NumericMatrix observation_impute_cpp_simple(NumericMatrix xbar, IntegerVector index_simple, NumericMatrix xtest, IntegerMatrix S);
RcppExport SEXP _PPtreeregViz_observation_impute_cpp_simple(SEXP xbarSEXP, SEXP index_simpleSEXP, SEXP xtestSEXP, SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type xbar(xbarSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type index_simple(index_simpleSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type xtest(xtestSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type S(SSEXP);
    rcpp_result_gen = Rcpp::wrap(observation_impute_cpp_simple(xbar, index_simple, xtest, S));
    return rcpp_result_gen;
END_RCPP
}
// weight_matrix_cpp
arma::mat weight_matrix_cpp(List subsets, int m, int n, NumericVector w);
RcppExport SEXP _PPtreeregViz_weight_matrix_cpp(SEXP subsetsSEXP, SEXP mSEXP, SEXP nSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type subsets(subsetsSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weight_matrix_cpp(subsets, m, n, w));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_PPtreeregViz_observation_impute_cpp_simple", (DL_FUNC) &_PPtreeregViz_observation_impute_cpp_simple, 4},
    {"_PPtreeregViz_weight_matrix_cpp", (DL_FUNC) &_PPtreeregViz_weight_matrix_cpp, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_PPtreeregViz(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
