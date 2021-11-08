#!/usr/bin/env zsh

set -ex
CIRCIFY=$(which compiler-exe)
SCRIPT_PATH="${0:A:h}"
BENCH_PATH=${SCRIPT_PATH}/pequin_bench

# 1 argument: benchmark name
# 2 argument: compiler
# 3 argument: number of constraints
function save_result() {
    echo $1,$2,$3,$4 >> $SCRIPT_PATH/results-wip.csv
}

function init_results() {
    rm -rf $SCRIPT_PATH/results-wip.csv
    echo benchmark,compiler,constraints,wall_time > $SCRIPT_PATH/results-wip.csv
}

function commit_results() {
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/all_results.csv
}

# 1 argument: c path
# 2 argument: compiler
# 3 argument: benchmark name
function count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    e=$(date +%s.%N)
    case $2 in
    circify)
        s=$(date +%s.%N)
        C_pequin_io=True C_no_overflow=True $CIRCIFY --emit-r1cs c compute $1
        e=$(date +%s.%N)
        ;;
    *)
        echo "Unknown circom compiler: $2"
        exit 1
        ;;
    esac
    n=$(head -n 1 C | awk '{print $3}')
    cd -
    save_result $3 $2 $n $(($e - $s))
    rm -rf $d
}
typeset -A paths
paths=(
            boyer_moore "$BENCH_PATH/boyer_occur_benes.c"
            kmp_search "$BENCH_PATH/kmpsearch_flat.c"
            mergesort "$BENCH_PATH/mergesort_benes.c"
            mm "$BENCH_PATH/mm.c"
            ptrchase "$BENCH_PATH/ptrchase_benes.c"
            rle_decode "$BENCH_PATH/rle_decode_flat.c"
            sparse_matvec "$BENCH_PATH/sparse_matvec_flat.c"
         )

init_results
for b in "${(@k)paths}"; do
    p="${paths[$b]}"
    echo $b $p
    for compiler in circify; do
        count $p $compiler $b
    done
done
commit_results
cat ./pequin_all_results.csv | tail -n +2 >> all_results.csv
#cat ./slow_results.csv | tail -n +2 >> all_results.csv

