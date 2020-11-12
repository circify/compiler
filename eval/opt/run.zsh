#!/usr/bin/env zsh
set -ex
ZOKRATES=$(which zokrates)
CIRCIFY=$(which compiler-exe)
SCRIPT_PATH="${0:A:h}"


#  1 argument: array size
#  2 argument: optimized?
#  3 argument: number of constraints
#  4 argument: z3 time
function save_result() {
    echo $1,$2,$3,$4 >> $SCRIPT_PATH/results-wip.csv
}

function init_results() {
    rm -rf $SCRIPT_PATH/results-wip.csv
    echo rounds,optimized,constraints,z3_time > $SCRIPT_PATH/results-wip.csv
}

function commit_results() {
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/results.csv
}

# First argument: size
# Second argument: optimized?
function count() {
    echo "rounds: $1, optimized: $2"
    d=$(mktemp -d -p . )
    cd $d
    touch C
    cfile=$(readlink -f "C")
    sed "s/ROUNDS/$1/" ../sha.c > tmp.c
    z3t=$(env C_pequin_io=True C_streams=time::z3 C_c_sv=True C_opt_z3=$2 $CIRCIFY -C $cfile c-check compute tmp.c | rg Time | awk '{ print $3 }')
    if [[ $2 = "True" ]]; then
        env C_smt_opts=cfee,arrayElim,flattenAnds,cfee C_pequin_io=True C_c_sv=True $CIRCIFY -C $cfile c-emit-r1cs main tmp.c
    else
        env C_smt_opts=cfee,arrayElim,flattenAnds C_pequin_io=True C_c_sv=True $CIRCIFY -C $cfile c-emit-r1cs main tmp.c
    fi
    n=$(head -n 1 $cfile | awk '{print $3}')
    cd -
    echo Time: $z3t N: $n
    save_result $1 $2 $n $z3t
    rm -rf $d
}

init_results
for s in $(seq 18 22); do
    for o in True False; do
        count $s $o
    done
done
commit_results

