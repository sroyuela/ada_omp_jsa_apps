#define DIM 8192
#define NB 256
#define BS DIM/NB

extern void GOMP_parallel_start(void (*)(void *), void *, unsigned int);
extern void GOMP_parallel_end(void);
extern _Bool GOMP_single_start();
extern void GOMP_task(void (*)(void *), void *, void (*)(void *, void *), long int, long int, _Bool, unsigned int, void **, int);
extern void GOMP_taskwait(void);
extern void GOMP_barrier(void);

void omp_potrf(double *A, int ts, int ld);
void omp_trsm(double *A, double *B, int ts, int ld);
void omp_syrk(double *A, double *B, int ts, int ld);
void omp_gemm(double *A, double *B, double *C, int ts, int ld);

void initialize_matrix(double *matrix);
void convert_to_blocks(double matrix[DIM][DIM], double *Ah[NB][NB]);
void convert_to_linear(double *Ah[NB][NB], double matrix[DIM][DIM]);
int check_factorization(double *A1, double *A2);

