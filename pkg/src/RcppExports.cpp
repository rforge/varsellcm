// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// OptimizeMICL
S4 OptimizeMICL(S4 reference, StringVector name);
RcppExport SEXP VarSelLCM_OptimizeMICL(SEXP referenceSEXP, SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< S4 >::type reference(referenceSEXP);
    Rcpp::traits::input_parameter< StringVector >::type name(nameSEXP);
    __result = Rcpp::wrap(OptimizeMICL(reference, name));
    return __result;
END_RCPP
}
// ComputeMICL
S4 ComputeMICL(S4 reference, StringVector name);
RcppExport SEXP VarSelLCM_ComputeMICL(SEXP referenceSEXP, SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< S4 >::type reference(referenceSEXP);
    Rcpp::traits::input_parameter< StringVector >::type name(nameSEXP);
    __result = Rcpp::wrap(ComputeMICL(reference, name));
    return __result;
END_RCPP
}
