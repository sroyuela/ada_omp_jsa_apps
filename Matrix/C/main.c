#include <stdlib.h>
#include <stdio.h>

#include <omp.h>

#include "timer.h"

#define Simul_Load 50000
#define Size 512

extern void GOMP_task(void (*)(void *), void *, void (*)(void *, void *), long int, long int, _Bool, unsigned int, void **, int);
extern void GOMP_taskwait();
extern void GOMP_parallel_start(void (*)(void *), void *, unsigned int);
extern void GOMP_parallel_end(void);

unsigned n_threads = 1;
unsigned n_tasks = 1;
   
// 1.- Kernels
void Heavy_Computation_Balanced (float* A)
{
    float Res = 0;
    for (int I=1; I<=Simul_Load; ++I)
        Res += *A * 2.0;
    *A = Res;
}
#if 0
void Heavy_Computation_Unbalanced (float* A)
{
    float Res = 0;
    int New_Simul_Load = Simul_Load * (rand()%4) / (int)*A;
    for (int I=1; I<=New_Simul_Load; ++I)
        Res += *A * 2.0;
    *A = Res;
}
#endif

// 2.- Sequential version
void Process_Seq(float ***M)
{
    for (int I=0; I<Size; ++I)
        for (int J=0; J<Size; ++J)
            Heavy_Computation_Balanced(&((*M)[I][J]));
}

// 3.- Parallel version
// 3.1.- Balanced
// 3.1.1- Task function
struct Task_Function_Args
{
    float ***M;
    int First_Pos;
    int Last_Pos;
};
void Task_Function_Balanced(struct Task_Function_Args* Task_Params)
{
    for (int I=Task_Params->First_Pos; I<=Task_Params->Last_Pos; ++I)
        for (int J=0; J<Size; ++J)
            Heavy_Computation_Balanced(&((*Task_Params->M)[I][J]));
}
// 3.1.2- Parallel function
void Process_Parallel_Balanced_Internal (float ***M)
{
//    for (int I=0; I<Size; ++I)
//        for (int J=0; J<Size; ++J)
//        {
//            struct Task_Function_Args Task_Data;
//            Task_Data.M = &((*M)[I][J]);
//            GOMP_task((void (*)(void *))Task_Function_Balanced, &Task_Data, 0, 8, 4, 1, 0, 0, 0);
//        }

    int First_Pos, Last_Pos;
    for (int K=0; K<n_tasks; ++K)
    {
        First_Pos = K * (Size / n_tasks);
        Last_Pos = (K+1) * (Size / n_tasks) - 1;
        if (K == n_tasks-1)
            Last_Pos = Size-1;

        struct Task_Function_Args Task_Data;
        Task_Data.M = M;
        Task_Data.First_Pos = First_Pos;
        Task_Data.Last_Pos = Last_Pos;
        GOMP_task((void (*)(void *))Task_Function_Balanced, &Task_Data, 0, 16, 4, 1, 0, 0, 0);
    }

    GOMP_taskwait();
}
struct Parallel_Function_Args
{
    float ***M;
};
void Parallel_Function_Balanced(struct Parallel_Function_Args* Parallel_Params)
{
    if (omp_get_thread_num() == 0)
        Process_Parallel_Balanced_Internal(Parallel_Params->M);
}
// 3.1.3.- Parallel entry
void Process_Parallel_Balanced (float ***M)
{
    struct Parallel_Function_Args Parallel_Data;
    Parallel_Data.M = M;
    GOMP_parallel_start((void (*)(void *))Parallel_Function_Balanced, &Parallel_Data, n_threads);
    Parallel_Function_Balanced(&Parallel_Data);
    GOMP_parallel_end();
}
#if 0
// 3.2.- Unbalanced
// 3.2.1- Task function
void Task_Function_Unbalanced(struct Task_Function_Args* Task_Params)
{
    Heavy_Computation_Unbalanced(Task_Params->M);
}
// 3.2.2- Parallel function
void Process_Parallel_Unbalanced_Internal (float ***M)
{
    for (int I=0; I<Size; ++I)
        for (int J=0; J<Size; ++J)
        {
            struct Task_Function_Args Task_Data;
            Task_Data.M = &((*M)[I][J]);
            GOMP_task((void (*)(void *))Task_Function_Unbalanced, &Task_Data, 0, 8, 4, 1, 0, 0, 0);
        }

    GOMP_taskwait();
}
void Parallel_Function_Unbalanced(struct Parallel_Function_Args* Parallel_Params)
{
    if (omp_get_thread_num() == 0)
        Process_Parallel_Unbalanced_Internal(Parallel_Params->M);
}
// 3.1.3.- Parallel entry
void Process_Parallel_Unbalanced (float ***M)
{
    struct Parallel_Function_Args Parallel_Data;
    Parallel_Data.M = M;
    GOMP_parallel_start((void (*)(void *))Parallel_Function_Unbalanced, &Parallel_Data, n_threads);
    Parallel_Function_Unbalanced(&Parallel_Data);
    GOMP_parallel_end();
}
#endif

// 4.- Auxiliary methods
void Init(float ***M)
{
    *M = malloc(sizeof(float*)*Size);
    for (int I=0; I<Size; ++I)
        (*M)[I] = malloc(sizeof(float)*Size);
    for (int I=0; I<Size; ++I)
        for (int J=0; J<Size; ++J)
            (*M)[I][J] = rand();
}

void Print_Matrix(float ***M)
{
    for (int I=0; I<Size; ++I)
        for (int J=0; J<Size; ++J)
            printf("%.6f,", (*M)[I][J]);
        printf("\n");
}

// 5.- Main function
int main(int argc, char** argv)
{
    float **M;

    if (argc != 4)
    {
        printf("Use: ./main.c n_threads n_tasks file.out\n");
        return -1;
    }
    n_threads = atoi(argv[1]);
    n_tasks = atoi(argv[2]);
    FILE *fp;
    fp = fopen(argv[3], "a");
   
#ifdef SEQ
    // Sequential
    {
        Init(&M);
        START_TIMER;
        Process_Seq(&M);
        END_TIMER;
        printf("Time for sequential: %lf (s)\n", TIMER);
        fprintf(fp, "%lf,", TIMER);
    }
#else
    // Parallel Balanced
    {
        Init(&M);
        START_TIMER;
        Process_Parallel_Balanced(&M);
        END_TIMER;
        printf("Time for parallel balanced (%u threads): %lf (s) (%d)\n", n_threads, TIMER);
        fprintf(fp, "%lf,", TIMER);
    }
#endif

    // Parallel Unbalanced
//     {
//         Init(&M);
//         START_TIMER;
//         Process_Parallel_Unbalanced(&M);
//         END_TIMER;
//     }

    fclose(fp);

    return 1;
}
