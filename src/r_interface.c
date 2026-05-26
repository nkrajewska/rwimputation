#include <R.h>
#include <Rinternals.h>
#include "impute_core.h"



SEXP r_rwimpute(SEXP r_matrix, SEXP r_p_matrix, SEXP r_col_types, SEXP r_parameters){
    int rows = nrows(r_matrix);
    int cols = ncols(r_matrix);
    
    double *data = REAL(r_matrix);
    double *p_matrix = REAL(r_p_matrix);

    int *col_types = INTEGER(r_col_types);

    double *params = REAL(r_parameters);

    SEXP r_result = PROTECT(allocMatrix(REALSXP, rows, cols));
    double *result = REAL(r_result);

    run_rw_algorithm(data, p_matrix, col_types, params, rows, cols, result);
    
    UNPROTECT(1);
    return r_result;
}