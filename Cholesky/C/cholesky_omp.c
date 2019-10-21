#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <omp.h>

#include "cholesky.h"

unsigned n_threads = 1;

// ****************************************************************************** //
// ************************** Wrappers for MKL kernels ************************** //

void omp_potrf(double * const A, int ts, int ld)
{
    static int INFO;
    static const char L = 'L';
    dpotrf_(&L, &ts, A, &ld, &INFO);
}

void omp_trsm(double *A, double *B, int ts, int ld)
{
    static char LO = 'L', TR = 'T', NU = 'N', RI = 'R';
    static double DONE = 1.0;
    dtrsm_(&RI, &LO, &TR, &NU, &ts, &ts, &DONE, A, &ld, B, &ld );
}

void omp_syrk(double *A, double *B, int ts, int ld)
{
    static char LO = 'L', NT = 'N';
    static double DONE = 1.0, DMONE = -1.0;
    dsyrk_(&LO, &NT, &ts, &ts, &DMONE, A, &ld, &DONE, B, &ld );
}

void omp_gemm(double *A, double *B, double *C, int ts, int ld)
{
    static const char TR = 'T', NT = 'N';
    static double DONE = 1.0, DMONE = -1.0;
    dgemm_(&NT, &TR, &ts, &ts, &ts, &DMONE, A, &ld, B, &ld, &DONE, C, &ld);
}

// ************************ END wrappers for MKL kernels ************************ //
// ****************************************************************************** //


// ****************************************************************************** //
// *********************** Sequential Cholesky computation ********************** //

#ifdef SEQ
void cholesky_seq(double *(*Ah)[NB])
{
    int ts = BS;
    //printf("Executing cholesky_blocked_seq\n");
    for (int k = 0; k < NB; k++) {

        // Diagonal Block factorization
        omp_potrf (Ah[k][k], ts, ts);

        // Triangular systems
        for (int i = k + 1; i < NB; i++) {
            omp_trsm (Ah[k][k], Ah[k][i], ts, ts);
        }

        // Update trailing matrix
        for (int i = k + 1; i < NB; i++) {
            for (int j = k + 1; j < i; j++) {
                omp_gemm (Ah[k][i], Ah[k][j], Ah[j][i], ts, ts);
            }
            omp_syrk (Ah[k][i], Ah[i][i], ts, ts);
        }
    }
}
#endif

// ********************* END Sequential Cholesky computation ******************** //
// ****************************************************************************** //


// ****************************************************************************** //
// ************************* OpenMP Cholesky computation ************************ //

#ifdef TASKS
void cholesky_tasks(double* Ah[NB][NB])
{
    #pragma omp parallel
    #pragma omp single
    {
        int ts = BS;
        for (int k = 0; k < NB; k++) {

            // Diagonal Block factorization
            omp_potrf (Ah[k][k], ts, ts);

            // Triangular systems
            for (int i = k + 1; i < NB; i++) {
                #pragma omp task
                omp_trsm (Ah[k][k], Ah[k][i], ts, ts);
            }
            #pragma omp taskwait

            // Update trailing matrix
            for (int i = k + 1; i < NB; i++) {
                for (int j = k + 1; j < i; j++) {
                    #pragma omp task
                    omp_gemm (Ah[k][i], Ah[k][j], Ah[j][i], ts, ts);
                }
                #pragma omp task
                omp_syrk (Ah[k][i], Ah[i][i], ts, ts);
            }
            #pragma omp taskwait
        }
    }
}
#endif

#ifdef TASKS_LOW
struct  args_1_t
{
    double* (*Ah)[NB];
    int k;
    int i;
};
void cholesky_tasks_ofloaded_task_1(struct args_1_t *args)
{
    double *(*Ah)[NB] = (*args).Ah;
    int k = (*args).k;
    int i = (*args).i;
    int ts = BS;
    omp_trsm(Ah[k][k], Ah[k][i], ts, ts);
}

struct  args_2_t
{
    double *(*Ah)[NB];
    int k;
    int i;
    int j;
};
void cholesky_tasks_ofloaded_task_2(struct args_2_t *args)
{
    double *(*Ah)[NB] = (*args).Ah;
    int k = (*args).k;
    int i = (*args).i;
    int j = (*args).j;
    int ts = BS;
    omp_gemm(Ah[k][i], Ah[k][j], Ah[j][i], ts, ts);
}

void cholesky_tasks_ofloaded_task_3(struct args_1_t *args)
{
    double *(*Ah)[NB] = (*args).Ah;
    int k = (*args).k;
    int i = (*args).i;
    int ts = BS;
    omp_syrk(Ah[k][i], Ah[i][i], ts, ts);
}

struct args_0_t
{
    double *(**Ah)[NB];
};
void cholesky_tasks_ofloaded_parallel(struct args_0_t *args)
{
    double* (*Ah)[NB] = *((double* (**)[NB])(*args).Ah);
    int ts = BS;
    {
        if (omp_get_thread_num() == 0)
        {
            for (int k = 0; k < NB; k++)
            {
                omp_potrf(Ah[k][k], ts, ts);
                for (int i = k + 1; i < NB; i++)
                {
                    struct args_1_t targs;
                    targs.Ah = Ah;
                    targs.k = k;
                    targs.i = i;
                    unsigned long int task_flags_1 = 0;
                    GOMP_task((void (*)(void *))cholesky_tasks_ofloaded_task_1, &targs, (void (*)(void *, void *))0, 24, 8, 1, task_flags_1, 0, 0);
                }

                GOMP_taskwait();

                for (int i = k + 1; i < NB; i++)
                {
                    for (int j = k + 1; j < i; j++)
                    {
                        struct args_2_t targs;
                        targs.Ah = Ah;
                        targs.k = k;
                        targs.i = i;
                        targs.j = j;
                        unsigned long int task_flags_2 = 0;
                        GOMP_task((void (*)(void *))cholesky_tasks_ofloaded_task_2, &targs, (void (*)(void *, void *))0, 32, 8, 1, task_flags_2, 0, 0);
                    }
                    struct args_1_t targs;
                    targs.Ah = Ah;
                    targs.k = k;
                    targs.i = i;
                    unsigned long int task_flags_3 = 0;
                    GOMP_task((void (*)(void *))cholesky_tasks_ofloaded_task_3, &targs, (void (*)(void *, void *))0, 24, 8, 1, task_flags_3, 0, 0);
                }
                GOMP_taskwait();
            }
        }
    }
}

void cholesky_tasks(double *Ah[NB][NB])
{
    //printf("Executing cholesky_tasks with %d threads\n", n_threads);
    struct args_0_t args;
    args.Ah = &Ah;
    GOMP_parallel_start((void (*)(void *))cholesky_tasks_ofloaded_parallel, &args, n_threads);
    cholesky_tasks_ofloaded_parallel(&args);
    GOMP_parallel_end();
}
#endif

#ifdef DEPS
void cholesky_deps(double* Ah[NB][NB])
{
    #pragma omp parallel
    #pragma omp single
    {
        int ts = BS;
        double (*AhDep)[NB][NB] = (double (*) [NB][NB])Ah;

        for (int k = 0; k < NB; k++) {

            // Diagonal Block factorization
            #pragma omp task depend(inout:Ah[k][k])
            omp_potrf (Ah[k][k], ts, ts);

            // Triangular systems
            for (int i = k + 1; i < NB; i++) {
                #pragma omp task depend(in:Ah[k][k]) depend(inout:Ah[k][i])
                omp_trsm (Ah[k][k], Ah[k][i], ts, ts);
            }

            // Update trailing matrix
            for (int i = k + 1; i < NB; i++) {
                for (int j = k + 1; j < i; j++) {
                    #pragma omp task depend(in:Ah[k][i]) depend(in:Ah[k][j]) depend(inout:Ah[j][i])
                    omp_gemm (Ah[k][i], Ah[k][j], Ah[j][i], ts, ts);
                }
                #pragma omp task depend(in:Ah[k][i]) depend(inout:Ah[i][i])
                omp_syrk (Ah[k][i], Ah[i][i], ts, ts);
            }

        }
    }
}
#endif

#ifdef DEPS_LOW
enum GOMP_task_flags
{
    GOMP_TASK_UNTIED = 1,
    GOMP_TASK_FINAL = 2,
    GOMP_TASK_MERGEABLE = 4,
    GOMP_TASK_DEPEND = 8
};

struct  args_3_t
{
    double *(*Ah)[NB];
    int k;
};
void cholesky_deps_ofloaded_task_1(struct args_3_t *args)
{
    double* (*Ah)[NB] = (*args).Ah;
    int k = (*args).k;
    int ts = BS;
    omp_potrf(Ah[k][k], ts, ts);
}

struct  args_4_t
{
    double *(*Ah)[NB];
    int k;
    int i;
};
void cholesky_deps_ofloaded_task_2(struct args_4_t *args)
{
    double *(*Ah)[NB] = (*args).Ah;
    int k = (*args).k;
    int i = (*args).i;
    int ts = BS;
    omp_trsm(Ah[k][k], Ah[k][i], ts, ts);
}

struct  args_5_t
{
    double *(*Ah)[NB];
    int k;
    int i;
    int j;
};
void cholesky_deps_ofloaded_task_3(struct args_5_t *args)
{
    double *(*Ah)[NB] = (*args).Ah;
    int k = (*args).k;
    int i = (*args).i;
    int j = (*args).j;
    int ts = BS;
    omp_gemm(Ah[k][i], Ah[k][j], Ah[j][i], ts, ts);
}

struct  args_6_t
{
    double *(*Ah)[NB];
    int k;
    int i;
};
void cholesky_deps_ofloaded_task_4(struct args_6_t *args)
{
    double *(*Ah)[NB] = (*args).Ah;
    int k = (*args).k;
    int i = (*args).i;
    int ts = BS;
    omp_syrk(Ah[k][i], Ah[i][i], ts, ts);
}

struct  args_7_t
{
    double * (**Ah)[NB];
};
void cholesky_deps_ofloaded_parallel(struct args_7_t *args)
{
    double * (*Ah)[NB] = *((double* (**)[NB])(*args).Ah);
    int ts = BS;
    if (omp_get_thread_num() == 0)
    {
        for (int k = 0; k < NB; k++)
        {
            struct args_3_t targs;
            targs.Ah = Ah;
            targs.k = k;
            unsigned long int task_flags_1 = 0;
            task_flags_1 |= GOMP_TASK_DEPEND;
//                 #pragma omp task depend(inout:AhDep[k][k])
            void *task_deps_1[3L] = {[0] = (void *)1U, [1] = (void *)1U, [2] = Ah[k][k]};
            GOMP_task((void (*)(void *))cholesky_deps_ofloaded_task_1, &targs, (void (*)(void *, void *))0, 20, 8, 1, task_flags_1, task_deps_1, 0);
            for (int i = k + 1; i < NB; i++)
            {
                struct args_4_t targs;
                targs.Ah = Ah;
                targs.k = k;
                targs.i = i;
                unsigned long int task_flags_2 = 0;
                task_flags_2 |= GOMP_TASK_DEPEND;
//                 #pragma omp task depend(in:AhDep[k][k]) depend(inout:AhDep[k][i])
                void *task_deps_2[4L] = {[0] = (void *)2U, [1] = (void *)1U, [2] = Ah[k][k], [3] = Ah[k][i]};
                GOMP_task((void (*)(void *))cholesky_deps_ofloaded_task_2, &targs, (void (*)(void *, void *))0, 24, 8, 1, task_flags_2, task_deps_2, 0);
            }
            for (int i = k + 1; i < NB; i++)
            {
                for (int j = k + 1; j < i; j++)
                {
                    struct args_5_t targs;
                    targs.Ah = Ah;
                    targs.k = k;
                    targs.i = i;
                    targs.j = j;
                    unsigned long int task_flags_3 = 0;
                    task_flags_3 |= GOMP_TASK_DEPEND;
//                     #pragma omp task depend(in:AhDep[k][i]) depend(in:AhDep[k][j]) depend(inout:AhDep[j][i])
                    void *task_deps_3[5L] = {[0] = (void *)3U, [1] = (void *)1U, [2] = Ah[k][j], [3] = Ah[k][j], [4] = Ah[j][i]};
                    GOMP_task((void (*)(void *))cholesky_deps_ofloaded_task_3, &targs, (void (*)(void *, void *))0, 32, 8, 1, task_flags_3, task_deps_3, 0);
                }
                struct args_6_t targs;
                targs.Ah = Ah;
                targs.k = k;
                targs.i = i;
                unsigned long int task_flags_4 = 0;
                task_flags_4 |= GOMP_TASK_DEPEND;
//                 #pragma omp task depend(in:AhDep[k][i]) depend(inout:AhDep[i][i])
                void *task_deps_4[4L] = {[0] = (void *)2U, [1] = (void *)1U, [2] = Ah[k][i], [3] = Ah[i][i]};
                GOMP_task((void (*)(void *))cholesky_deps_ofloaded_task_4, &targs, (void (*)(void *, void *))0, 24, 8, 1, task_flags_4, task_deps_4, 0);
            }
        }
    }
}

void cholesky_deps(double* Ah[NB][NB])
{
    //printf("Executing cholesky_deps with %d threads\n", n_threads);
    struct args_7_t args;
    args.Ah = &Ah;
    GOMP_parallel_start((void (*)(void *))cholesky_deps_ofloaded_parallel, &args, n_threads);
    cholesky_deps_ofloaded_parallel(&args);
    GOMP_parallel_end();
}
#endif

// *********************** END OpenMP Cholesky computation ********************** //
// ****************************************************************************** //



// ****************************************************************************** //
// ******************************** Main function ******************************* //

int main(int argc, char* argv[])
{
    if (argc != 3)
    {
        printf("Use: ./exec n_threads file.out\n");
        return -1;
    }
#ifdef SEQ
    n_threads = 1;
#elif defined(TASKS_LOW) || defined(DEPS_LOW)
    n_threads = atoi(argv[1]);
#else
    n_threads = omp_get_max_threads();
#endif
    
    FILE *fp;
    fp = fopen(argv[2], "a");

    const double eps = BLAS_dfpinfo( blas_eps );
    const int  n = DIM; // matrix size
    const int ts = BS; 	// tile size
    const int nt = NB;	// number of tiles

    // Allocate and initialize linear matrices
    double * const matrix = (double *) malloc(n * n * sizeof(double));
    assert(matrix != NULL);
    initialize_matrix(n, ts, matrix);
 
    double * const original_matrix = (double *) malloc(n * n * sizeof(double));
    assert(original_matrix != NULL);

    for (int i = 0; i < n * n; i++ ) {
        original_matrix[i] = matrix[i];
    }

    // Allocate and initialize blocked matrix
    double *Ah[NB][NB];
    for (int i = 0; i < nt; i++) {
        for (int j = 0; j < nt; j++) {
           Ah[i][j] = malloc(ts * ts * sizeof(double));
           assert(Ah[i][j] != NULL);
        }
    }
    convert_to_blocks(ts, nt, n, (double(*)[n]) matrix, Ah);

    int i;
    float t1, t2;
#ifdef SEQ
    t1 = get_time();
    cholesky_seq(Ah);
    t2 = get_time() - t1;
    printf("seq: %lf (s)\n", t2);
    fprintf(fp, "%lf,", t2);
#endif

#if defined(TASKS) || defined(TASKS_LOW)
    t1 = get_time();
    cholesky_tasks(Ah);
    t2 = get_time() - t1;
    printf("NTH %d, omp tasks: %lf (s)\n", n_threads, t2);
    fprintf(fp, "%lf,", t2);
#endif

#if defined(DEPS) || defined(DEPS_LOW)
    t1 = get_time();
    cholesky_deps(Ah);
    t2 = get_time() - t1;
    printf("NTH %d, omp deps: %lf (s)\n", n_threads, t2);
    fprintf(fp, "%lf,", t2);
#endif

     convert_to_linear(ts, nt, n, Ah, (double (*)[n]) matrix);

 #ifdef CHECK
     const char uplo = 'L';
     check_factorization(n, original_matrix, matrix, n, uplo, eps);
 #endif
   
    // Free matrices
    for (int i = 0; i < nt; i++) {
        for (int j = 0; j < nt; j++) {
           assert(Ah[i][j] != NULL);
           free(Ah[i][j]);
        }
    }
    free(original_matrix);
    free(matrix);

    return 0;
}

// ****************************** END Main function ***************************** //
// ****************************************************************************** //
