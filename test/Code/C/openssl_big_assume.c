#define BN_ULONG uint64_t
#define BN_UMULT_LOHI(low,high,a,b) ({  \
        unsigned __int128 ret=(unsigned __int128)(a)*(b); \
        (high)=ret>>64; (low)=ret;      })
        //uint64_t ret=(uint64_t)(a)*(b); \

//Broken
#define mul_add_c2(a,b,c0,c1,c2) {     \
       BN_ULONG ta=(a),tb=(b),t0,t1,t2;\
       BN_UMULT_LOHI(t0,t1,ta,tb);     \
       t2 = t1+t1; c2 += (t2<t1)?1:0;  \
       t1 = t0+t0; t2 += (t1<t0)?1:0;  \
       /* critical line */ __VERIFIER_assume(t2 + 1 == 0); \
       c0 += t1; t2 += (c0<t1)?1:0;    \
       c1 += t2; c2 += (c1<t2)?1:0;    \
       }

#include "stdio.h"
#include "stdint.h"

int mult(BN_ULONG a, BN_ULONG b, BN_ULONG c0, BN_ULONG c1, BN_ULONG c2) {
    // Copy inputs so we can check correctness w.r.t. them
    BN_ULONG aa = a, bb = b, cc0 = c0, cc1 = c1, cc2 = c2;
    // Use macro
    mul_add_c2(a, b, c0, c1, c2);
    // Assert correctness
    // (c2 || c1 || c0) == (cc2 || cc1 || cc0) + 2 * aa * bb
    "__SMT_assert: Eq (DynBvNaryExpr BvAdd 192 [ DynBvConcat 192 (DynBvConcat 128 (Var \"cc2\" (SortBv 64)) (Var \"cc1\" (SortBv 64))) (Var \"cc0\" (SortBv 64)), DynBvNaryExpr BvMul 192 [ DynBvLit [192]2, DynBvUext 192 128 (Var \"aa\" (SortBv 64)), DynBvUext 192 128 (Var \"bb\" (SortBv 64))]]) (DynBvConcat 192 (DynBvConcat 128 (Var \"c2\" (SortBv 64)) (Var \"c1\" (SortBv 64))) (Var \"c0\" (SortBv 64)))";
    return 0;
}
