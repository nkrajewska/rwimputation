#include <R.h>
#include <Rinternals.h>
#include "impute_core.h"



SEXP r_rwimpute(SEXP r_matrix, SEXP r_rows, SEXP r_col_ind,SEXP r_wght, SEXP r_col_types, SEXP r_parameters, SEXP r_seed){
    int rows = nrows(r_matrix);
    int cols = ncols(r_matrix);
    
    double *data = REAL(r_matrix);

    int *p_rows = INTEGER(r_rows);
    int *p_col_ind = INTEGER(r_col_ind);
    double *p_wght = REAL(r_wght);

    int *col_types = INTEGER(r_col_types);
    double *params = REAL(r_parameters);

    SEXP r_result = PROTECT(allocMatrix(REALSXP, rows, cols));
    double *result = REAL(r_result);
    unsigned int base_seed = (unsigned int)asInteger(r_seed);
    run_rw_algorithm(data, p_rows, p_col_ind, p_wght, col_types, params, rows, cols, result, base_seed);
    
    UNPROTECT(1);
    return r_result;
}