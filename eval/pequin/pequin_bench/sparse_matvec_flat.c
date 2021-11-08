typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef long int int64_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long int uint64_t;
typedef signed char int_least8_t;
typedef short int int_least16_t;
typedef int int_least32_t;
typedef long int int_least64_t;
typedef unsigned char uint_least8_t;
typedef unsigned short int uint_least16_t;
typedef unsigned int uint_least32_t;
typedef unsigned long int uint_least64_t;
typedef signed char int_fast8_t;
typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
typedef unsigned char uint_fast8_t;
typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
typedef long int intptr_t;
typedef unsigned long int uintptr_t;
typedef long int intmax_t;
typedef unsigned long int uintmax_t;
struct In {
  int vector[5];
  int elms[10];
  int inds[10];
  int ptrs[6];
};
struct Out {
  int out[5];
};
void compute(struct In *input, struct Out *output) {
  int i;
  int j;
  for (i = 0; i < 5; i++) {
    output->out[i] = 0;
  }
  { /****begin flattened loop****/
    int ip0_int_0000000;
    int ip1_int_0000000;
    int iej_int_0000000;
    int inj_int_0000000;
    int vij_int_0000000;
    int state_0000000 = 0;
    int count_0000000 = 0;
    while (count_0000000 < 14) {
      /*for(i = 0; i < 5; i++) */

      if (state_0000000 == 0) {
        i = 0;
        if (i < 5) {
          state_0000000++;
        } else {
          state_0000000 = 6;
        }
      }

      if (state_0000000 == 1) {
        // int ip0 =
        ip0_int_0000000 = input->ptrs[i];
        //;

        // int ip1 =
        ip1_int_0000000 = input->ptrs[i + 1];
        //;

        state_0000000++;
      }
      /*for(j = ip0_int_0000000; j < ip1_int_0000000; j++) */

      if (state_0000000 == 2) {
        j = ip0_int_0000000;
        if (j < ip1_int_0000000) {
          state_0000000++;
        } else {
          state_0000000 = 5;
        }
      }

      if (state_0000000 == 3) {
        // int iej =
        iej_int_0000000 = input->elms[j];
        //;

        // int inj =
        inj_int_0000000 = input->inds[j];
        //;

        // int vij =
        vij_int_0000000 = input->vector[inj_int_0000000];
        //;

        output->out[i] += iej_int_0000000 * vij_int_0000000;

        state_0000000++;
      }

      if (state_0000000 == 4) {
        j++;
        if (j < ip1_int_0000000) {
          state_0000000 = 3;
        } else {
          state_0000000++;
        }
      }

      if (state_0000000 == 5) {
        i++;
        if (i < 5) {
          state_0000000 = 1;
        } else {
          state_0000000++;
        }
      }

      count_0000000++;
    }
  }
}
