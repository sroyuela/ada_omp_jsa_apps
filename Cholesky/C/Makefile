PROGRAM	    	= cholesky

CC = gcc

OMP_FLAGS	= -fopenmp
EXTRA		= -ggdb -O0

#LDFLAGS		= -L/home/sroyuela/Software/Libraries/Mkl/ins/mkl/lib/intel64
LDFLAGS         = -L/apps/INTEL/2017.4/mkl/lib/intel64


LIBS  		= -lmkl_sequential -lmkl_core -lmkl_rt -lm
#INCS  		= -I/home/sroyuela/Software/Libraries/Mkl/ins/mkl/include
INCS		= -I/apps/INTEL/2017.4/mkl/include

TARGETS     	= $(PROGRAM)_seq $(PROGRAM)_omp_deps $(PROGRAM)_omp_tasks

all: clean $(TARGETS)

$(PROGRAM)_seq: $(PROGRAM)_omp.c
	$(CC) -DVERBOSE -DSEQ $(CPPFLAGS) $(INCS) $+ -o $@ $(LDFLAGS) $(LIBS)

$(PROGRAM)_omp_tasks: $(PROGRAM)_omp.c
	$(CC) -DVERBOSE -DTASK $(EXTRA) $(CPPFLAGS) $(OMP_FLAGS) $(INCS) $+ -o $@ $(LDFLAGS) $(LIBS) 
	
$(PROGRAM)_omp_deps: $(PROGRAM)_omp.c
	$(CC) -DVERBOSE -DDEPS $(CPPFLAGS) $(OMP_FLAGS) $(INCS) $+ -o $@ $(LDFLAGS) $(LIBS) 

.PHONY: clean
clean:
	rm -rf $(TARGETS) *.o
