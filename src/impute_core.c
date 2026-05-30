#include "impute_core.h"
#include <R.h>
#include <stdlib.h>
#include <omp.h>

int sample_next_node(const double *prob_row, int num_nodes, double rand_val) {
    double cum_prob = 0.0;
    for (int k = 0; k < num_nodes; k++) {
        cum_prob += prob_row[k];
        if (rand_val <= cum_prob) {
            return k; 
        }
    }
    return num_nodes - 1; 
}

double calculate_average(double *vect, int n){
    double sum = 0.0;
    int count = 0;
    for(int i = 0; i <n; i++){
        if(!ISNA(vect[i])){
            sum += vect[i];
            count++;
        }
    }
    if(!count) return NA_REAL;
    return sum/count;
}
double calculate_mode(double *vect, int n){
    int counts[n];
    int vals[n];
    int n_uniq = 0;

    for(int i = 0; i <n; i++){
        if(!ISNA(vect[i])){
            int val = (int)vect[i];
            
            int flag =  0;
            for(int j=0; j< n_uniq; j++){
                if(vals[j] == val){
                    counts[j]++;
                    flag = 1;
                    break;
                }
            }

            if(!flag){
                counts[n_uniq] =1;
                vals[n_uniq] = val;
                n_uniq++;
            }
        }
    }
    if(!n_uniq) return NA_REAL;    

    int mode = vals[0];
    int max_counts = counts[0];
    
    for(int i = 1; i < n_uniq; i++){
        if(counts[i] > max_counts){
            max_counts = counts[i];
            mode = vals[i];
        }
    }
    return (double)mode;
}

double simulate_random_walk(int start_node, const double* p_matrix, const double *data, int target_col, 
    int num_rows, int num_steps, unsigned int* seed, int col_type){
    double *trail = (double*)malloc(num_steps*sizeof(double));
    int curr_node = start_node;

    for(int i = 0; i < num_steps; i++){
        double rand_val = (double)rand_r(seed) / RAND_MAX;
        curr_node = sample_next_node(&p_matrix[curr_node * num_rows], num_rows, rand_val);
        trail[i] = data[curr_node + target_col * num_rows];
    }
    double fin_val;
    if (col_type){
        fin_val = calculate_mode(trail, num_steps);
    }else{
        fin_val = calculate_average(trail, num_steps);
    }
    free(trail);
    return fin_val;
}



void run_rw_algorithm(const double *data, const double *p_matrix, const int *col_types, 
                      const double *params, int rows, int cols, double *result, unsigned int base_seed){
    int num_steps = (int)params[0];
    int max_threads = omp_get_max_threads();
    unsigned int *seeds = malloc(max_threads * sizeof(unsigned int));
    for (int t = 0; t < max_threads; t++) seeds[t] = base_seed + (unsigned int)t*2654435761u;

    #pragma omp parallel for collapse(2) schedule(dynamic)
    for (int j = 0; j < cols; j++) {
        for (int i = 0; i < rows; i++) {
            
            int index = i + j * rows; 
            
            if (ISNA(data[index])) {
                int t_id = omp_get_thread_num();
                result[index] = simulate_random_walk(i, p_matrix, data, j, rows, num_steps, &seeds[t_id], col_types[j]);
            } else {
                result[index] = data[index];
            }
        }
    }
}