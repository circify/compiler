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
  char compressed[20];
};
struct Out {
  char decompressed[10];
};
void compute(struct In *input, struct Out *output) {
  int i;
  int j;
  int outp = 0;
  { /****begin flattened loop****/
    int data_int_0000000;
    int len_int_0000000;
    int state_0000000 = 0;
    int count_0000000 = 0;
    int breakGuard_0000000 = 0;
    while (count_0000000 < 10) {
      /*for(i = 0; outp < 10; i += 2) */

      if (state_0000000 == 0) {
        i = 0;
        if (outp < 10) {
          state_0000000++;
        } else {
          state_0000000 = 6;
        }
      }

      if (state_0000000 == 1) {
        // int data =
        data_int_0000000 = input->compressed[i];
        //;

        // int len =
        len_int_0000000 = input->compressed[i + 1];
        //;

        state_0000000++;
      }
      /*for(j = 0; j <= len_int_0000000; j++) */

      if (state_0000000 == 2) {
        j = 0;
        if (j <= len_int_0000000) {
          state_0000000++;
        } else {
          state_0000000 = 5;
        }
      }

      if (state_0000000 == 3) {
        output->decompressed[outp] = data_int_0000000;
        outp++;
        if (outp >= 10) { /*break;*/
          state_0000000 = 4;
          breakGuard_0000000 = 1;
        }
        breakGuard_0000000 = 0;

        state_0000000++;
      }

      if (state_0000000 == 4) {
        j++;
        if (j <= len_int_0000000) {
          state_0000000 = 3;
        } else {
          state_0000000++;
        }
      }

      if (state_0000000 == 5) {
        i += 2;
        if (outp < 10) {
          state_0000000 = 1;
        } else {
          state_0000000++;
        }
      }

      count_0000000++;
    }
  }
}
