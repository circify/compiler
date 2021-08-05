set -ex
CIRCOM=$(which circom)
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
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/all_results.csv
}

# First argument: circom circuit path
function circify_count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    e=$(date +%s.%N)
    cd -
    save_result $1 circify $n $(($e- $s))
    rm -rf $d
}

# First argument: circom circuit path
function circom_count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    $CIRCOM -o circuit.json $1
    n=$(jq '.constraints|length' circuit.json)
    e=$(date +%s.%N)
    cd -
    rm -rf $d
}

# First argument: circom path
# Second argument: compiler
# Third argument: benchmark name
function count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    case $2 in
    circom)
        $CIRCOM -o circuit.json $1
        n=$(jq '.constraints|length' circuit.json)
        ;;
    circify)
        $CIRCIFY circom $1 --emit-r1cs -C C
        n=$(head -n 1 C | awk '{print $3}')
        ;;
    *)
        echo "Unknown circom compiler: $2"
        exit 1
        ;;
    esac
    e=$(date +%s.%N)
    cd -
    save_result $4 $1 $2 $n $(($e - $s))
    rm -rf $d
}
tests=(
    aliascheck_test.circom
    babyadd_tester.circom
    babycheck_test.circom
    babypbk_test.circom
    binsub_test.circom
    constants_test.circom
    ##eddsa_test.circom
    eddsamimc_test.circom
    eddsaposeidon_test.circom
    edwards2montgomery.circom
    ##escalarmul_min_test.circom
    ##escalarmul_test.circom
    ##escalarmul_test_min.circom
    escalarmulany_test.circom
    escalarmulfix_test.circom
    ##escalarmulw4table_test.circom
    ##escalarmulw4table_test3.circom
    greatereqthan.circom
    greaterthan.circom
    isequal.circom
    iszero.circom
    lesseqthan.circom
    lessthan.circom
    mimc_sponge_hash_test.circom
    mimc_sponge_test.circom
    mimc_test.circom
    montgomery2edwards.circom
    montgomeryadd.circom
    montgomerydouble.circom
    mux1_1.circom
    mux2_1.circom
    mux3_1.circom
    mux4_1.circom
    pedersen2_test.circom
    ##pedersen_test.circom
    sha256_2_test.circom
    #sha256_test448.circom
    #sha256_test512.circom
    sign_test.circom
    smtprocessor10_test.circom
    smtverifier10_test.circom
    sum_test.circom
)
init_results
for b in $(seq ${#tests}); do
    name=${tests[$b]}
    p="$HOME/repos/llcl/circomlib/test/circuits/${tests[$b]}"
    for compiler in circom circify; do
        count $p $compiler $b $name
    done
done
commit_results

