
#include <omp.h>
#include <stdlib.h>

#include "lu.h"

enum GOMP_task_flags
{
    GOMP_TASK_UNTIED = 1,
    GOMP_TASK_FINAL = 2,
    GOMP_TASK_MERGEABLE = 4,
    GOMP_TASK_DEPEND = 8
};

void * ptr_1U = (void*) 1U;
void * ptr_2U = (void*) 2U;
void * ptr_3U = (void*) 3U;



#if 0
void sparselu_task(float M[S][S][BS][BS], unsigned n_threads)
{
    int ii, jj, kk;

    #pragma omp parallel private(kk,ii,jj) shared(M)  num_threads(n_threads)
    #pragma omp master
    for (kk=0; kk<S; kk++)
    {
        #pragma omp task firstprivate(kk) shared(M)
        lu0(M[kk][kk]);
        #pragma omp taskwait

        for (jj=kk+1; jj<S; jj++)
            #pragma omp task firstprivate(kk, jj) shared(M)
            fwd(M[kk][kk], M[kk][jj]);
        for (ii=kk+1; ii<S; ii++)
            #pragma omp task firstprivate(kk, ii) shared(M)
            bdiv (M[kk][kk], M[ii][kk]);
        #pragma omp taskwait

        for (ii=kk+1; ii<S; ii++)
            for (jj=kk+1; jj<S; jj++)
                #pragma omp task firstprivate(kk, jj, ii) shared(M)
                bmod(M[ii][kk], M[kk][jj], M[ii][jj]);
        #pragma omp taskwait
    }
}
#endif

#if 1

// CODE AND DATA FOR TASK 2
struct  tasks_args_2
{
    float (**M)[S][BS][BS];
    int kk;
    int jj;
};
void tasks_task_2(struct tasks_args_2 *args)
{
    float (**const M)[S][BS][BS] = &(*(*args).M);
    int kk = (*args).kk;
  int jj = (*args).jj;
 // printf("Code of task with KK = %d and JJ = %d\n", kk, jj);
  fwd((*M)[kk][kk], (*M)[kk][jj]);
   
}

// CODE AND DATA FOR TASK 3
struct  tasks_args_3
{
    float (**M)[S][BS][BS];
    int kk;
    int ii;
};
void tasks_task_3(struct tasks_args_3 *args)
{
    float (**const M)[S][BS][BS] = &(*(*args).M);
    int kk = (*args).kk;
    int ii = (*args).ii;
    bdiv((*M)[kk][kk], (*M)[ii][kk]);
}

// CODE AND DATA FOR TASK 4
struct  tasks_args_4
{
    float (**M)[S][BS][BS];
    int kk;
    int jj;
    int ii;
};
void tasks_task_4(struct tasks_args_4 *args)
{
    float (**const M)[S][BS][BS] = &(*(*args).M);
    int ii = (*args).ii;
    int kk = (*args).kk;
    int jj = (*args).jj;
    bmod((*M)[ii][kk], (*M)[kk][jj], (*M)[ii][jj]);
}

// CODE AND DATA FOR PARALLEL REGION
struct  tasks_parallel_args
{
    float (**M)[S][BS][BS];
};
void tasks_parallel_code(struct tasks_parallel_args *args)
{
    int kk, jj, ii;
    float (**const M)[S][BS][BS] = &(*(*args).M);

    if (omp_get_thread_num() == 0)
    {
        for (kk=0; kk<S; kk++)
        {
            lu0((*M)[kk][kk]);

            for (jj=kk+1; jj<S; jj++)
            {
                struct tasks_args_2 args_2;
                args_2.M = (float (**)[S][BS][BS]) M;
                args_2.kk = kk;
                args_2.jj = jj;
                unsigned long int task_flags_2 = 0;
                task_flags_2 |= GOMP_TASK_UNTIED;
                GOMP_task((void (*)(void *))tasks_task_2, &args_2, (void (*)(void *, void *))0, 16, 8, 1, task_flags_2, 0, 0);
            }
            for (ii=kk+1; ii<S; ii++)
            {
                struct tasks_args_3 args_3;
                args_3.M = (float (**)[S][BS][BS]) M;
                args_3.kk = kk;
                args_3.ii = ii;
                unsigned long int task_flags_3 = 0;
                task_flags_3 |= GOMP_TASK_UNTIED;
                GOMP_task((void (*)(void *))tasks_task_3, &args_3, (void (*)(void *, void *))0, 16, 8, 1, task_flags_3, 0, 0);
            }

            GOMP_taskwait();
            for (ii=kk+1; ii<S; ii++)
                for (jj=kk+1; jj<S; jj++)
                {
                    struct tasks_args_4 args_4;
                    args_4.M = (float (**)[S][BS][BS]) M;
                    args_4.kk = kk;
                    args_4.jj = jj;
                    args_4.ii = ii;
                    unsigned long int task_flags_4 = 0;
                    task_flags_4 |= GOMP_TASK_UNTIED;
                    GOMP_task((void (*)(void *))tasks_task_4, &args_4, (void (*)(void *, void *))0, 24, 8, 1, task_flags_4, 0, 0);
                }
            GOMP_taskwait();
        }
    }
}

void sparselu_task(float (*M)[S][BS][BS], unsigned n_threads)
{
    struct tasks_parallel_args args;
    args.M = &M;
    GOMP_parallel_start((void (*)(void *))tasks_parallel_code, &args, n_threads);
    tasks_parallel_code(&args);
    GOMP_parallel_end();
}

#endif



#if 0
void sparselu_deps(float M[S][S][BS][BS], unsigned n_threads)
{
    int ii, jj, kk;

    #pragma omp parallel private(kk,ii,jj) shared(M) num_threads(n_threads)
    #pragma omp master
    {
        /*#pragma omp task untied*/
        for (kk=0; kk<S; kk++)
        {
            #pragma omp task firstprivate(kk) shared(M) \
                             depend(inout: M[kk][kk])
            lu0(M[kk][kk]);
            for (jj=kk+1; jj<S; jj++)
                #pragma omp task firstprivate(kk, jj) shared(M) \
                                depend(in: M[kk][kk]) \
                                depend(inout: M[kk][jj])
                fwd(M[kk][kk], M[kk][jj]);
            for (ii=kk+1; ii<S; ii++)
                #pragma omp task firstprivate(kk, ii) shared(M) \
                                    depend(in: M[kk][kk]) \
                                    depend(inout: M[ii][kk])
                bdiv (M[kk][kk], M[ii][kk]);

            for (ii=kk+1; ii<S; ii++)
                for (jj=kk+1; jj<S; jj++)
                    #pragma omp task firstprivate(kk, jj, ii) shared(M) \
                                        depend(in: M[ii][kk], M[kk][jj]) \
                                        depend(inout: M[ii][jj])
                    bmod(M[ii][kk], M[kk][jj], M[ii][jj]);

        }
    }
}
#endif

#if 1
// CODE AND DATA FOR TASK 1
struct deps_task_args_1
{
    float (**M)[S][BS][BS];
    int kk;
};
void deps_task_1(struct deps_task_args_1 *args)
{
  float (**const M)[S][BS][BS] = &(*(*args).M);
  int kk = (*args).kk;
  lu0((*M)[kk][kk]);
}

// CODE AND DATA FOR TASK 2
struct  deps_task_args_2
{
    float (**M)[S][BS][BS];
    int kk;
    int jj;
};
void deps_task_2(struct deps_task_args_2 *args)
{
  float (**const M)[S][BS][BS] = &(*(*args).M);
  int kk = (*args).kk;
  int jj = (*args).jj;
  fwd((*M)[kk][kk], (*M)[kk][jj]);
}

// CODE AND DATA FOR TASK 3
struct  deps_task_args_3
{
    float (**M)[S][BS][BS];
    int kk;
    int ii;
};
static void deps_task_3(struct deps_task_args_3 *args)
{
  float (**const M)[S][BS][BS] = &(*(*args).M);
  int kk = (*args).kk;
  int ii = (*args).ii;
  bdiv((*M)[kk][kk], (*M)[ii][kk]);
}

// CODE AND DATA FOR TASK 4
struct  deps_task_args_4
{
    float (**M)[S][BS][BS];
    int kk;
    int jj;
    int ii;
};
void deps_task_4(struct deps_task_args_4 *args)
{
  float (**const M)[S][BS][BS] = &(*(*args).M);
  int ii = (*args).ii;
  int kk = (*args).kk;
  int jj = (*args).jj;
  bmod((*M)[ii][kk], (*M)[kk][jj], (*M)[ii][jj]);
}

// CODE AND DATA FOR PARALLEL REGION
struct  task_deps_parallel_args
{
    float (**M)[S][BS][BS];
};
void deps_parallel_code(struct task_deps_parallel_args *args)
{
    int kk, jj, ii;
    float (**const M)[S][BS][BS] = &(*(*args).M);

    if (omp_get_thread_num() == 0)
    {
        for (kk=0; kk<S; kk++)
        {
            struct deps_task_args_1 args_1;
            args_1.M = (float (**)[S][BS][BS]) M;
            args_1.kk = kk;
            void *task_deps_1[3L] = {[0] = (void *)1U, [1] = (void *)1U, [2] = (*M)[kk][kk]};
            unsigned long int task_flags_1 = 0;
            task_flags_1 |= GOMP_TASK_UNTIED;
            task_flags_1 |= GOMP_TASK_DEPEND;
            GOMP_task((void (*)(void *))deps_task_1, &args_1, (void (*)(void *, void *))0, 16, 8, 1, task_flags_1, task_deps_1, 0);
            
            for (jj=kk+1; jj<S; jj++)
                if ((*M)[kk][jj] != (void *)0)
                {
                    struct deps_task_args_2 args_2;
                    args_2.M = (float (**)[S][BS][BS]) M;
                    args_2.kk = kk;
                    args_2.jj = jj;
                    void *task_deps_2[4L] = {[0] = (void *)2U, [1] = (void *)1U, [2] = (*M)[kk][kk], [3] = (*M)[kk][jj]};
                    unsigned long int task_flags_2 = 0;
                    task_flags_2 |= GOMP_TASK_UNTIED;
                    task_flags_2 |= GOMP_TASK_DEPEND;
                    GOMP_task((void (*)(void *))deps_task_2, &args_2, (void (*)(void *, void *))0, 16, 8, 1, task_flags_2, task_deps_2, 0);
                }
            for (ii=kk+1; ii<S; ii++)
            {
                struct deps_task_args_3 args_3;
                args_3.M = (float (**)[S][BS][BS]) M;
                args_3.kk = kk;
                args_3.ii = ii;
                void *task_deps_3[4L] = {[0] = (void *)2U, [1] = (void *)1U, [2] = (*M)[kk][kk], [3] = (*M)[ii][kk]};
                unsigned long int task_flags_3 = 0;
                task_flags_3 |= GOMP_TASK_UNTIED;
                task_flags_3 |= GOMP_TASK_DEPEND;
                GOMP_task((void (*)(void *))deps_task_3, &args_3, (void (*)(void *, void *))0, 16, 8, 1, task_flags_3, task_deps_3, 0);
            }

            for (ii=kk+1; ii<S; ii++)
                for (jj=kk+1; jj<S; jj++)
                {
                    struct deps_task_args_4 args_4;
                    args_4.M = (float (**)[S][BS][BS]) M;
                    args_4.kk = kk;
                    args_4.jj = jj;
                    args_4.ii = ii;
                    void *task_deps_4[5L] = {[0] = (void *)3U, [1] = (void *)1U, [2] = (*M)[ii][kk], [3] = (*M)[kk][jj], [4] = (*M)[ii][jj]};
                    unsigned long int task_flags_4 = 0;
                    task_flags_4 |= GOMP_TASK_DEPEND;
                    task_flags_4 |= GOMP_TASK_UNTIED;
                    GOMP_task((void (*)(void *))deps_task_4, &args_4, (void (*)(void *, void *))0, 24, 8, 1, task_flags_4, task_deps_4, 0);
                }
        }
    }
}

void sparselu_deps(float (*M)[S][BS][BS], unsigned n_threads)
{
    struct task_deps_parallel_args args;
    args.M = &M;
    GOMP_parallel_start((void (*)(void *))deps_parallel_code, &args, n_threads);
    deps_parallel_code(&args);
    GOMP_parallel_end();
}
#endif

