#include <stdint.h>

/*
  Matrix multiplication.
 */

//Constants
#define IDX(r,c) (SIZE*(r) + (c))

struct In {
  uint16_t A[900]; uint16_t B[900];
};

struct Out {
  uint64_t C[900];
};

void compute(struct In* input, struct Out* output) {
  int i,j,k;

  for(i = 0; i < 30; i++){
    for(j = 0; j < 30; j++){
      uint64_t C_ij = 0;
      for(k = 0; k < 30; k++){
        C_ij += input->A[30*i+k] * input->B[30*k+j];
      }
      output->C[30*i+j] = C_ij;
    }
  }
}
