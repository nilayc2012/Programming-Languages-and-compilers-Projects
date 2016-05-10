#/*
# **********************************************
# *  515 Programming Languages and Compilers I *
# *  Spring 2016                               *
# *  Authors: Ulrich Kremer                    *
# *********************************************
# */

CCFLAGS = -O3 -ggdb -fopenmp -lrt -lm
CUDAFLAGS = -O3 -lrt -lm

all: 	run_seq \
	run_omp \
	run_cuda

run_seq: redblack-1d.c
	gcc redblack-1d.c $(CCFLAGS) -o run_seq

run_omp: redblack-1d-omp.c
	gcc redblack-1d-omp.c $(CCFLAGS) -o run_omp

run_cuda: redblack-1d-cuda.cu
	nvcc redblack-1d-cuda.cu $(CUDAFLAGS) -o run_cuda

run_cuda_shared: redblack-1d-cuda_sharedmemory.cu
	nvcc redblack-1d-cuda_sharedmemory.cu $(CUDAFLAGS) -o run_cuda_shared

clean:
	rm -rf *.o
	rm -rf run_seq
	rm -rf run_omp
	rm -rf run_cuda
	rm -rf run_cuda_shared
