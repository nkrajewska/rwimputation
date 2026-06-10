#include "impute_core.h"
#include <R.h>
#include <stdlib.h>

#ifdef _OPENMP
  #include <omp.h>
#endif

static inline int sample_next_node(const int *ind, const double* wghts, double rand_val, int k) {
    double cum_prob = 0.0;

    for (int i = 0; i < k; i++) {
        cum_prob += wghts[i];
        if (rand_val <= cum_prob) {
            return ind[i]; 
        }
    }
    return ind[k - 1]; 
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
unsigned int rand_gen(unsigned int *seed) {
    *seed = (*seed * 1103515245 + 12345);
    return *seed;
}

double simulate_random_walk(int start_node,const int* rows, const int* col_ind, const double* wghts,
    const double *data, int target_col, int num_rows, int num_steps, unsigned int loc_seed, int col_type, double restart_prob, double* trail){
        
    int curr_node = start_node;
    const double* targ_col_ptr = &data[target_col*num_rows];


    if(restart_prob > 0.0){
        for(int i =0; i < num_steps; i++){
            unsigned int rand = rand_gen(&loc_seed);
            double rnd = (double)(rand % 65536) / 65535.0;
            if(rnd<restart_prob) curr_node = start_node;
            
            int start = rows[curr_node];
            
            int deg = rows[curr_node + 1] - start;
            unsigned int rand2 = rand_gen(&loc_seed);
            double rand_val = (double)(rand2 % 65536) / 65535.0;

            curr_node = sample_next_node(col_ind + start, wghts + start, rand_val, deg);
            trail[i] = targ_col_ptr[curr_node];

        }
    }else{
        for(int i = 0; i< num_steps;i++){
            unsigned int rand2 = rand_gen(&loc_seed);
            double rand_val = (double)(rand2 % 65536) / 65535.0;

            int start = rows[curr_node];
            int deg= rows[curr_node + 1] - start;

            curr_node = sample_next_node(col_ind + start, wghts + start, rand_val, deg);
            trail[i] = targ_col_ptr[curr_node];
        }
    }

    double fin_val;
    if (col_type){
        fin_val = calculate_mode(trail, num_steps);
    }else{
        fin_val = calculate_average(trail, num_steps);
    }
    return fin_val;
}



void run_rw_algorithm(const double *data, const int* rows,const int* col_ind, const double* wghts,  const int *col_types, 
                      const double *params, int num_rows, int num_cols, double *result, unsigned int base_seed){
    int num_steps = (int)params[0];
    double restart_prob = params[1];



    #pragma omp parallel
    {
        double *trail =(double*)malloc(num_steps*sizeof(double));

        #pragma omp for collapse(2) schedule(dynamic)
        for (int j = 0; j < num_cols; j++) {
            for (int i = 0; i < num_rows; i++) {
            
                int index = i + j * num_rows; 
            
                if (ISNA(data[index])) {
                    unsigned int seed = base_seed + (i * 1103515245u) + (j * 12345u);
                    if (seed == 0) seed = 1;
                    result[index] = simulate_random_walk(i, rows, col_ind, wghts, data, j, num_rows, num_steps, 
                        seed, col_types[j], restart_prob, trail);
                } else result[index] = data[index];
            }
        }
        free(trail);
    }
}