
#include <stdlib.h>
#include <stdio.h>
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

void changed_lu0(float (*M)[S][BS][BS], int kk)
{
    int i, j, k;

    for (k=0; k<BS; k++)
        for (i=k+1; i<BS; i++)
        {
            M[kk][kk][i][k] = M[kk][kk][i][k] / M[kk][kk][k][k];
            for (j=k+1; j<BS; j++)
                M[kk][kk][i][j] = M[kk][kk][i][j] - M[kk][kk][i][k] * M[kk][kk][k][j];
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

void changed_bdiv(float (*M)[S][BS][BS], int kk, int ii)
{
    int i, j, k;
    for (i=0; i<BS; i++)
        for (k=0; k<BS; k++)
        {
            M[ii][kk][i][k] = M[ii][kk][i][k] / M[kk][kk][k][k];
            for (j=k+1; j<BS; j++)
                M[ii][kk][i][j] = M[ii][kk][i][j] - M[ii][kk][i][k]*M[kk][kk][k][j];
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

void changed_bmod(float (*M)[S][BS][BS], int kk, int ii, int jj)
{
    int i, j, k;
    for (i=0; i<BS; i++)
        for (j=0; j<BS; j++)
            for (k=0; k<BS; k++)
                M[ii][jj][i][j] = M[ii][jj][i][j] - M[ii][kk][i][k]*M[kk][jj][k][j];
}




void fwd(float diag[BS][BS], float col[BS][BS])
{
  int i, j, k;
  
  for (j=0; j<BS; j++)
    for (k=0; k<BS; k++)
      for (i=k+1; i<BS; i++)
	{
	//  printf("fwd with i = %d, j = %d and k= %d\n", i, j, k);
	  col[i][j] = col[i][j] - diag[i][k]*col[k][j];
	}
}

void changed_fwd(float (*M)[S][BS][BS],int kk, int jj)
{
  int i, j, k;
  
  for (j=0; j<BS; j++)
    for (k=0; k<BS; k++)
      for (i=k+1; i<BS; i++)
	{
	//  printf("fwd with i = %d, j = %d and k= %d\n", i, j, k);
	  M[kk][jj][i][j] = M[kk][jj][i][j] - M[kk][kk][i][k]*M[kk][jj][k][j];
	}
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

void sparselu_print(float M[S][S][BS][BS])
{
    int i, j, ii, jj;


    for (ii=0; ii < S; ii++)
    {
        for (jj=0; jj < S; jj++)
        {
            for (i = 0; i < BS; i++)
            {
                for (j = 0; j < BS; j++)
                {
                    printf(" %f",M[ii][jj][i][j]);
                }
	    }
	  printf("\n");
        }
    }
}





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

void sparselu_seq(float (*M)[S][BS][BS])
{
    int ii, jj, kk;
    for (kk=0; kk<S; kk++)
    {
        changed_lu0(M,kk);

        for (jj=kk+1; jj<S; jj++)
            changed_fwd(M, kk, jj);
        for (ii=kk+1; ii<S; ii++)
            changed_bdiv (M, kk, ii);
        for (ii=kk+1; ii<S; ii++)
            for (jj=kk+1; jj<S; jj++)
                changed_bmod(M, kk, ii, jj);
    }
}
