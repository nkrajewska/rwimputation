#ifndef IMPUTE_CORE_H
#define IMPUTE_CORE_H

void run_rw_algorithm(
    const double *data, 
    const int* rows,
    const int* col_ind,
    const double* wghts,
    const int *col_types, 
    const double *params, 
    int num_rows, 
    int num_cols, 
    double *result,
    unsigned int base_seed
);

#endif 