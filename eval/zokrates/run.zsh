#!/usr/bin/env zsh
set -ex
ZOKRATES=$(which zokrates)
CIRCIFY=$(which compiler-exe)
SCRIPT_PATH="${0:A:h}"


# First argument: benchmark name
# Second argument: circom path
# Third argument: compiler
# Fourth argument: number of constraints
# Fifth argument: wall time
function save_result() {
    echo $1,$2,$3,$4,$5 >> $SCRIPT_PATH/results-wip.csv
}

function init_results() {
    rm -rf $SCRIPT_PATH/results-wip.csv
    echo benchmark,path,compiler,constraints,wall_time > $SCRIPT_PATH/results-wip.csv
}

function commit_results() {
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/results.csv
}

# First argument: circom path
# Second argument: compiler
# Third argument: benchmark name
function count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    case $2 in
    zokrates)
        n=$($ZOKRATES compile --light -i $1 | rg -o -w '\d+')
        ;;
    circify)
        touch C
        cfile=$(readlink -f "C")
        (cd ../../.. && C_inputs_in_range=False $CIRCIFY -C $cfile zokrates-emit-r1cs main $1)
        n=$(head -n 1 C | awk '{print $3}')
        ;;
    *)
        echo "Unknown circom compiler: $2"
        exit 1
        ;;
    esac
    e=$(date +%s.%N)
    cd -
    save_result $3 $1 $2 $n $(($e - $s))
    rm -rf $d
}
typeset -A assoc_array
assoc_array=(mimc7
             ~/repos/llcl/compiler/test/Code/Zokrates/stdlib/hashes/mimc7/mimc7R50.zok
             pedersen
             ~/repos/llcl/compiler/test/Code/Zokrates/stdlib/hashes/pedersen/512bit.zok
             ecc
             ~/repos/llcl/compiler/test/Code/Zokrates/stdlib/ecc/edwardsScalarMult.zok
             sha2round
             ~/repos/llcl/compiler/test/Code/Zokrates/stdlib/hashes/sha256/shaRound.zok
         )

init_results
for b in "${(@k)assoc_array}"; do
    p="${assoc_array[$b]}"
    for compiler in circify zokrates; do
        count $p $compiler $b
    done
done
commit_results

