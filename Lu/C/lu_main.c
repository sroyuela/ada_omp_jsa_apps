// SparseLU C kernels using OpenMP

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <libgen.h>

#include "lu.h"
#include "timer.h"

int main(int argc, char** argv)
{
    if (argc != 3)
    {
        printf("Use: ./exec n_threads file.out\n");
        return -1;
    }
    unsigned n_threads = atoi(argv[1]);
    FILE *fp;
    fp = fopen(argv[2], "a");

    float M[S][S][BS][BS];
    sparselu_init(M);

#ifdef SEQ
    START_TIMER;
    sparselu_seq(M);
    END_TIMER;
    printf("Time for sequential: %lf (s)\n", TIMER);
    fprintf(fp, "%lf,", TIMER);
#else
#ifdef TASK
    START_TIMER;
    sparselu_task(M, n_threads);
    END_TIMER;
    printf("Time for parallel tasks: %lf (s)\n", TIMER);
    fprintf(fp, "%lf,", TIMER);
#else	// DEPS
#ifdef DEPS
    START_TIMER;
    sparselu_deps(M, n_threads);
    END_TIMER;
    printf("Time for parallel deps: %lf (s)\n", TIMER);
    fprintf(fp, "%lf,", TIMER);
#endif
#endif
#endif

fclose(fp);

#ifdef CHECK
    float M_SEQ[S][S][BS][BS];
    sparselu_init(M_SEQ);
    sparselu_seq(M_SEQ);
    sparselu_check(M_SEQ, M);
#endif

    return 0;
}
