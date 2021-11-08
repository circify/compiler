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
  char needle[16];
  char haystack[64];
};
struct Out {
  int match;
};
void compute(struct In *input, struct Out *output) {
  int k;
  int fail[16];
  fail[0] = -1;
  fail[1] = 0;
  int tpos;
  int cand = 0;
  { /****begin flattened loop****/
    char nj_char_0000000;
    int state_0000000 = 0;
    int count_0000000 = 0;
    while (count_0000000 < 32) {
      /*for(tpos = 2; tpos < 16; tpos++) */

      if (state_0000000 == 0) {
        tpos = 2;
        if (tpos < 16) {
          state_0000000++;
        } else {
          state_0000000 = 7;
        }
      }

      if (state_0000000 == 1) {
        // char nj =
        nj_char_0000000 = input->needle[tpos - 1];
        //;

        state_0000000++;
      }
      /*while (cand > 0 && nj_char_0000000 != input->needle[cand]) */

      if (state_0000000 == 2) {
        if (cand > 0 && nj_char_0000000 != input->needle[cand]) {
          state_0000000++;
        } else {
          state_0000000 = 5;
        }
      }

      if (state_0000000 == 3) {
        cand = fail[cand];

        state_0000000++;
      }

      if (state_0000000 == 4) {
        if (cand > 0 && nj_char_0000000 != input->needle[cand]) {
          state_0000000 = 3;
        } else {
          state_0000000++;
        }
      }

      if (state_0000000 == 5) {
        if (cand < 1) {
          fail[tpos] = 0;
        } else {
          cand++;
          fail[tpos] = cand;
        }

        state_0000000++;
      }

      if (state_0000000 == 6) {
        tpos++;
        if (tpos < 16) {
          state_0000000 = 1;
        } else {
          state_0000000++;
        }
      }

      count_0000000++;
    }
  }

  output->match = 64;
  int i = 0;
  int m = 0;
  int last = 16 - 1;
  int end = 64 - last;
  { /****begin flattened loop****/
    int fi_int_0000000;
    int state_0000001 = 0;
    int count_0000001 = 0;
    int breakGuard_0000001 = 0;
    while (count_0000001 < 128) {
      /*while (m < end) */

      if (state_0000001 == 0) {
        if (m < end) {
          state_0000001++;
        } else {
          state_0000001 = 3;
        }
      }

      if (state_0000001 == 1) {
        if (input->needle[i] == input->haystack[m + i]) {
          if (i == last) {
            output->match = m;
            /*break;*/
            state_0000001 = 2;
            breakGuard_0000001 = 1;
          }

          if (!breakGuard_0000001) {
            i++;
          }
        } else {
          // int fi =
          fi_int_0000000 = fail[i];
          //;

          if (fi_int_0000000 > 0) {
            i = fail[i];
            fi_int_0000000 = fail[i];
            m = m + i - fi_int_0000000;
          } else {
            i = 0;
            m++;
          }
        }
        breakGuard_0000001 = 0;

        state_0000001++;
      }

      if (state_0000001 == 2) {
        if (m < end) {
          state_0000001 = 1;
        } else {
          state_0000001++;
        }
      }

      count_0000001++;
    }
  }
}
