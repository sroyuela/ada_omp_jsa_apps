CC = gcc
CFLAGS = -fopenmp

SRC = lu_main.c lu_kernels.c lu_utils.c

all: seq tasks tasks_check deps deps_check

seq:
	${CC} ${CFLAGS} -DSEQ -o lu_seq ${SRC}
tasks: 
	${CC} ${CFLAGS} -DTASK -o lu_task ${SRC}
tasks_check:
	${CC} ${CFLAGS} -DTASK -DCHECK -o lu_task_check ${SRC}
deps:
	${CC} ${CFLAGS} -DDEPS -o lu_deps ${SRC}
deps_check:
	${CC} ${CFLAGS} -DDEPS -DCHECK -o lu_deps_check ${SRC}

.PHONY: clean
clean:
	rm -f *.o lu_seq lu_task lu_task_check lu_deps lu_deps_check
