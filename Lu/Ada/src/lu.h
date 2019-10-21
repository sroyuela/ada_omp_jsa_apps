#define EPSILON 1.0E-6

// 64 / 128
#define S 64
#define BS 32

// In Ada there are equivalent S and BS defined in main.adb - these need also to be updated if S and BS are changed

extern void GOMP_parallel_start(void (*)(void *), void *, unsigned int);
extern void GOMP_parallel_end(void);
extern _Bool GOMP_single_start();
extern void GOMP_task(void (*)(void *), void *, void (*)(void *, void *), long int, long int, _Bool, unsigned int, void **, int);
extern void GOMP_taskwait(void);
extern void GOMP_barrier(void);

void lu0(float diag[BS][BS]);
void bdiv(float diag[BS][BS], float row[BS][BS]);
void bmod(float row[BS][BS], float col[BS][BS], float inner[BS][BS]);
void fwd(float diag[BS][BS], float col[BS][BS]);

void changed_lu0(float (*M)[S][BS][BS], int kk);
void changed_bdiv(float (*M)[S][BS][BS], int kk, int ii);
void changed_bmod(float (*M)[S][BS][BS], int kk, int ii, int jj);
void changed_fwd(float (*M)[S][BS][BS],int kk, int jj);


void sparselu_init (float M[S][S][BS][BS]);
void sparselu_print(float M[S][S][BS][BS]);


int sparselu_check(float M_SEQ[S][S][BS][BS], float M[S][S][BS][BS]);
void sparselu_seq(float M[S][S][BS][BS]);
void sparselu_task(float M[S][S][BS][BS], unsigned n_threads);
void sparselu_deps(float M[S][S][BS][BS], unsigned n_threads);
