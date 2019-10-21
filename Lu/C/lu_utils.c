
#include <stdlib.h>

#include "lu.h"

void lu0(float diag[BS][BS])
{
    int i, j, k;

    for (k=0; k<BS; k++)
        for (i=k+1; i<BS; i++)
        {
            diag[i][k] = diag[i][k] / diag[k][k];
            for (j=k+1; j<BS; j++)
                diag[i][j] = diag[i][j] - diag[i][k] * diag[k][j];
        }
}

void bdiv(float diag[BS][BS], float row[BS][BS])
{
    int i, j, k;
    for (i=0; i<BS; i++)
        for (k=0; k<BS; k++)
        {
            row[i][k] = row[i][k] / diag[k][k];
            for (j=k+1; j<BS; j++)
                row[i][j] = row[i][j] - row[i][k]*diag[k][j];
        }
}

void bmod(float row[BS][BS], float col[BS][BS], float inner[BS][BS])
{
    int i, j, k;
    for (i=0; i<BS; i++)
        for (j=0; j<BS; j++)
            for (k=0; k<BS; k++)
                inner[i][j] = inner[i][j] - row[i][k]*col[k][j];
}

void fwd(float diag[BS][BS], float col[BS][BS])
{
    int i, j, k;
    for (j=0; j<BS; j++)
        for (k=0; k<BS; k++)
            for (i=k+1; i<BS; i++)
                col[i][j] = col[i][j] - diag[i][k]*col[k][j];
}

void sparselu_init(float M[S][S][BS][BS])
{
    int i, j, ii, jj;

    float init_val = (float)((((3125 * 1325) % 65536) - 32768.0) / 16384.0);

    /* generating the structure */
    for (ii=0; ii < S; ii++)
    {
        for (jj=0; jj < S; jj++)
        {
            for (i = 0; i < BS; i++)
            {
                for (j = 0; j < BS; j++)
                {
                    M[ii][jj][i][j] = init_val;
                }
            }
        }
    }
}

#ifdef CHECK
#include <stdio.h>

int sparselu_check(float M[S][S][BS][BS], float N[S][S][BS][BS])
{
   int ii,jj,i,j;
   float r_err;

   for (ii=0; ii<S; ii++)
   {
      for (jj=0; jj<S; jj++)
      {
        for (i = 0; i < BS; i++)
        {
            for (j = 0; j < BS; j++)
            {
                r_err = M[ii][jj][i][j] - N[ii][jj][i][j];
                if ( r_err == 0.0 ) continue;

                if (r_err < 0.0 ) r_err = -r_err;

                if ( M[ii][jj][i][j] == 0 ) 
                {
                    printf("Checking failure: A[%d][%d]=%f  B[%d][%d]=%f; \n",
                            i,j, M[ii][jj][i][j], i,j, N[ii][jj][i][j]);
                    return 0;
                }
                r_err = r_err / M[ii][jj][i][j];
                if (r_err > EPSILON)
                {
                    printf("Checking failure: A[%d][%d]=%f  B[%d][%d]=%f; Relative Error=%f\n",
                            i,j, M[ii][jj][i][j], i,j, N[ii][jj][i][j], r_err);
                    return 0;
                }
            }
        }
      }
   }
   
   return 1;
}

#endif

#if defined CHECK || defined SEQ
void sparselu_seq(float (*M)[S][BS][BS])
{
    int ii, jj, kk;
    for (kk=0; kk<S; kk++)
    {
        lu0(M[kk][kk]);

        for (jj=kk+1; jj<S; jj++)
            fwd(M[kk][kk], M[kk][jj]);
        for (ii=kk+1; ii<S; ii++)
            bdiv (M[kk][kk], M[ii][kk]);
        for (ii=kk+1; ii<S; ii++)
            for (jj=kk+1; jj<S; jj++)
                bmod(M[ii][kk], M[kk][jj], M[ii][jj]);
    }
}
#endif
