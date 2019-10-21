#include <math.h>
//#include </home/sroyuela/Software/Libraries/Mkl/ins/mkl/include/mkl.h>
#include </apps/INTEL/2017.4/mkl/include/mkl.h>

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cholesky.h"


void * ptr_1U = (void*) 1U;
void * ptr_2U = (void*) 2U;
void * ptr_3U = (void*) 3U;

// ****************************************************************************** //
// ************************** Wrappers for MKL kernels ************************** //

void omp_potrf(double* A, int ts, int ld)
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
// *********************************** Utils ************************************ //

void add_to_diag(double * matrix, const int n, const double alpha)
{
    int i;
    for (i = 0; i < n; i++)
        matrix[ i + i * n ] += alpha;
}

void initialize_matrix(double *matrix)
{
    int ISEED[4] = {0,0,0,1};
    int intONE=1;

    int n = DIM;
    int i, j;
    for (i = 0; i < n*n; i+=n) {
        dlarnv_(&intONE, &ISEED[0], &n, &matrix[i]);
    }

    for (i=0; i<n; i++) {
        for (j=0; j<n; j++) {
            matrix[j*n + i] = matrix[j*n + i] + matrix[i*n + j];
            matrix[i*n + j] = matrix[j*n + i];
        }
    }

    add_to_diag(matrix, n, (double) n);
}

void gather_block(double *matrix, double *Ah)
{
    int i, j;
    for (i = 0; i < BS; i++)
        for (j = 0; j < BS; j++)
            Ah[i*BS + j] = matrix[i*DIM + j];
}

void convert_to_blocks(double matrix[DIM][DIM], double *Ah[NB][NB])
{
    int i, j, k;
    for (i = 0; i < NB; i++)
        for (j = 0; j < NB; j++)
            gather_block(&matrix[i*BS][j*BS], Ah[i][j]);
}

void scatter_block(double *A, double *matrix)
{
    int i, j, k;
    for (i = 0; i < BS; i++)
        for (j = 0; j < BS; j++)
            matrix[i*DIM + j] = A[i*BS + j];
}

void convert_to_linear(double *Ah[NB][NB], double matrix[DIM][DIM])
{
    int i, j, k;
    for (i = 0; i < NB; i++)
        for (j = 0; j < NB; j++)
            scatter_block(Ah[i][j], (double *) &matrix[i*BS][j*BS]);
}

enum blas_cmach_type {
            blas_base      = 151,
            blas_t         = 152,
            blas_rnd       = 153,
            blas_ieee      = 154,
            blas_emin      = 155,
            blas_emax      = 156,
            blas_eps       = 157,
            blas_prec      = 158,
            blas_underflow = 159,
            blas_overflow  = 160,
            blas_sfmin     = 161};

double BLAS_dpow_di(double x, int n)
{
    double rv = 1.0;

    if (n < 0) {
        n = -n;
        x = 1.0 / x;
    }

    for (; n; n >>= 1, x *= x) {
        if (n & 1)
            rv *= x;
    }

    return rv;
}

void BLAS_error(char *rname, int err, int val, int x)
{
    fprintf( stderr, "%s %d %d %d\n", rname, err, val, x );
    abort();
}

double BLAS_dfpinfo(enum blas_cmach_type cmach)
{
    const double b = 2.0;
    const int t = 53, l = 1024, m = -1021;
    char rname[] = "BLAS_dfpinfo";

    // for (i = 0; i < t; ++i) eps *= half;
    const double eps = BLAS_dpow_di( b, -t );
    // for (i = 0; i >= m; --i) r *= half;
    const double r = BLAS_dpow_di( b, m-1 );

    double o = 1.0;
    o -= eps;
    // for (i = 0; i < l; ++i) o *= b;
    o = (o * BLAS_dpow_di( b, l-1 )) * b;

    switch (cmach) {
        case blas_eps: return eps;
        case blas_sfmin: return r;
        default:
            BLAS_error( rname, -1, cmach, 0 );
            break;
    }
    return 0.0;
}

int check_factorization(double *A1, double *A2)
{
	char NORM = 'I', ALL = 'A', UP = 'U', LO = 'L', TR = 'T', NU = 'N', RI = 'R';
        const double eps = BLAS_dfpinfo( blas_eps );
        int LDA = DIM;
        int N = DIM;

#ifdef VERBOSE
	printf ("Checking result ...\n");
#endif

	double *Residual = (double *)malloc(N*N*sizeof(double));
	double *L1       = (double *)malloc(N*N*sizeof(double));
	double *L2       = (double *)malloc(N*N*sizeof(double));
	double *work     = (double *)malloc(N*sizeof(double));

	memset((void*)L1, 0, N*N*sizeof(double));
	memset((void*)L2, 0, N*N*sizeof(double));

	double alpha= 1.0;

	dlacpy_(&ALL, &N, &N, A1, &LDA, Residual, &N);

	/* Dealing with L'L or U'U  */
        dlacpy_(&LO, &N, &N, A2, &LDA, L1, &N);
        dlacpy_(&LO, &N, &N, A2, &LDA, L2, &N);
        dtrmm_(&RI, &LO, &TR, &NU, &N, &N, &alpha, L1, &N, L2, &N);

	/* Compute the Residual || A -L'L|| */
        int i, j;
	for (i = 0; i < N; i++)
            for (j = 0; j < N; j++)
                Residual[j*N+i] = L2[j*N+i] - Residual[j*N+i];

	double Rnorm = dlange_(&NORM, &N, &N, Residual, &N, work);
	double Anorm = dlange_(&NORM, &N, &N, A1, &N, work);

#ifdef VERBOSE
	printf("============\n");
	printf("Checking the Cholesky Factorization \n");
	printf("-- ||L'L-A||_oo/(||A||_oo.N.eps) = %e \n",Rnorm/(Anorm*N*eps));
#endif

	const int info_factorization = isnan(Rnorm/(Anorm*N*eps))
                                       || isinf(Rnorm/(Anorm*N*eps))
                                       || (Rnorm/(Anorm*N*eps) > 60.0);

// #ifdef VERBOSE
	if ( info_factorization){
            printf("\n-- Factorization is suspicious ! \n\n");
	}
	else{
            printf("\n-- Factorization is CORRECT ! \n\n");
	}
// #endif

	free(Residual); free(L1); free(L2); free(work);

	return info_factorization;
}

// ********************************* END Utils ********************************** //
// ****************************************************************************** //
