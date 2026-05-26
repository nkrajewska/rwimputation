#ifndef IMPUTE_CORE_H
#define IMPUTE_CORE_H

void run_rw_algorithm(
    const double *data, 
    const double *p_matrix, 
    const int *col_types, 
    const double *params, 
    int rows, 
    int cols, 
    double *result
);

#endif 