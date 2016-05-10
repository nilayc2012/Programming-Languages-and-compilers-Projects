/**
 * redblack-1d.c: U. Kremer for cs515, April 2016
 *
 */
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <time.h>

#define DATA_TYPE double
#define TIME_STEPS 1000
#define N 7500000

/* Array initialization. */
static
void init_array (DATA_TYPE *a)
{
  int i;

  for (i = 0; i < N; i++) {
    a[i] = (DATA_TYPE) random() /  N; 
  }
}


/* Must scan some live-out data so compiler cannot 
   use dead code elimination (DCE).
   Can be used also to check the correctness of the output. */
static
void print_array_elem(int i, DATA_TYPE *a)
{
  if ((0 <= i) && (i <= N))  
    fprintf(stderr, "(%d):%0.2lf\n", i, a[i]);
  else
    fprintf(stderr, "Error: out of bounds access \n");
}

static
void print_array(DATA_TYPE *a)
{
  int i;

  for (i = 0; i < N; i++) {
    fprintf(stderr, "(%d):%0.2lf  ", i, a[i]);
    if ((i+1)% 5 == 0) fprintf(stderr, "\n");
  }
  fprintf(stderr, "\n");
}


/* Main computational kernel. The whole function will be timed,
   including the call and return. */
static
void kernel_redblack_1d(int tsteps,
			DATA_TYPE *a)
{
  int t, i;

  for (t = 0; t < TIME_STEPS; t++) {

      for (i = 1; i < N - 1; i = i+2) { 
 	a[i] = 0.25 * (a[i-1] + 2* a[i] + a[i+1]);
      }

      for (i = 1; i < N - 2; i = i+2) { 
	a[i+1] = 0.25 * (a[i] + 2* a[i+1] + a[i+2]);
      }
      
  }
}



int main(void)
{
  int tsteps = TIME_STEPS;
  
  /* Timing */
  double start, end, diff;

  /* Variable declaration/allocation. */
  int nbytes = N*sizeof(DATA_TYPE);
  DATA_TYPE *a_h;

  a_h = (DATA_TYPE *) malloc( nbytes );
  
  /* Initialize array(s). */
  
  init_array (a_h);
  print_array_elem(N/2, a_h);
  print_array_elem(N/3, a_h);
  print_array_elem(N/4, a_h);
  print_array_elem(N/5, a_h);
  print_array_elem(N/6, a_h);
  
  /* Start timer. */
  start = clock();

  /* Run kernel. */
  kernel_redblack_1d (tsteps, a_h); 

  /* Stop timer and print */
  end = clock();
  diff = (end - start) / 1000000;
  printf("Execution time (CPU only): %f\n", diff);

  /* debugging and/or trick the dead code eliminator */
  /* print_array(a_h); */
  print_array_elem(N/2, a_h);
  print_array_elem(N/3, a_h);
  print_array_elem(N/4, a_h);
  print_array_elem(N/5, a_h);
  print_array_elem(N/6, a_h);

  /* free(a_h); */

  return 0;
}
