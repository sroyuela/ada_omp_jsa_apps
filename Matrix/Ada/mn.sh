#!/bin/bash

#SBATCH --job-name=matrix
#SBATCH -D .
#SBATCH --output=%j.out
#SBATCH --error=%j.err
#SBATCH --cpus-per-task=48
#SBATCH --ntasks=1
#SBATCH --time=08:00:00
#SBATCH --qos=bsc_cs


gprbuild default.gpr

for i in 48 24 16 8 4 2
do
  for j in 16 32 64 128 256 512
  do
    for k in {1..10}
   do
      ./obj/main omp $i $j matrix_ada_omp_${i}th_${j}t.csv
    done
  done

  for k in {1..10}
  do
    ./obj/main paraffin $i 16 matrix_ada_paraffin_${i}th.csv
  done
done

for i in {1..10}
do
    ./obj/main seq 1 1 matrix_ada_seq.csv
done

for j in 16 32 64 128 256 512
do
  for k in {1..10}
  do
    ./obj/main tasks 1 $j matrix_ada_tasks_${j}t.csv
  done
done