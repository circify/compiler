#!/usr/bin/env zsh
set -ex
ZOKRATES=$(which zokrates)
CIRCIFY=$(which compiler-exe)
SCRIPT_PATH="${0:A:h}"

cd $SCRIPT_PATH


# Second argument: circom path
# Third argument: compiler
# Fourth argument: number of constraints
# Fifth argument: wall time
function save_result() {
    echo $1,$2,$3,$4 >> $SCRIPT_PATH/results-wip.csv
}

function init_results() {
    rm -rf $SCRIPT_PATH/results-wip.csv
    echo path,compiler,constraints,wall_time > $SCRIPT_PATH/results-wip.csv
}

function commit_results() {
    mv $SCRIPT_PATH/results-wip.csv $SCRIPT_PATH/all_results.csv
}

# First argument: circom path
# Second argument: compiler
# Third argument: benchmark name
function count() {
    d=$(mktemp -d -p . )
    cd $d
    s=$(date +%s.%N)
    pfile=$(readlink -f $SCRIPT_PATH/../../$1)
    case $2 in
    zokrates)
        n=$($ZOKRATES compile --light -i $pfile | rg -o -w '\d+')
        ;;
    circify)
        touch C
        cfile=$(readlink -f "C")
        if [[ ( "$pfile" =~ .*sha256.* ) || ( "$pfile" =~ .*verify.* ) ]]
        then
            (cd ../../.. && pwd && C_inputs_in_range=False C_array_thresh=4000 $CIRCIFY -C $cfile --emit-r1cs zokrates main $pfile)
        else
            (cd ../../.. && pwd && C_inputs_in_range=False $CIRCIFY -C $cfile --emit-r1cs zokrates main $pfile)
        fi
        n=$(head -n 1 C | awk '{print $3}')
        ;;
    *)
        echo "Unknown circom compiler: $2"
        exit 1
        ;;
    esac
    e=$(date +%s.%N)
    cd -
    save_result $1 $2 $n $(($e - $s))
    rm -rf $d
}

benches=(
    test/Code/Zokrates/stdlib/ecc/babyjubjubParams.zok
    test/Code/Zokrates/stdlib/ecc/edwardsAdd.zok
    test/Code/Zokrates/stdlib/ecc/edwardsCompress.zok
    test/Code/Zokrates/stdlib/ecc/edwardsNegate.zok
    test/Code/Zokrates/stdlib/ecc/edwardsOnCurve.zok
    test/Code/Zokrates/stdlib/ecc/edwardsOrderCheck.zok
    test/Code/Zokrates/stdlib/ecc/edwardsScalarMult.zok
    test/Code/Zokrates/stdlib/ecc/proofOfOwnership.zok
    test/Code/Zokrates/stdlib/hashes/mimc7/mimc7R10.zok
    test/Code/Zokrates/stdlib/hashes/mimc7/mimc7R20.zok
    test/Code/Zokrates/stdlib/hashes/mimc7/mimc7R50.zok
    test/Code/Zokrates/stdlib/hashes/mimc7/mimc7R90.zok
    test/Code/Zokrates/stdlib/hashes/mimcSponge/mimcFeistel.zok
    test/Code/Zokrates/stdlib/hashes/mimcSponge/mimcSponge.zok
    test/Code/Zokrates/stdlib/hashes/pedersen/512bit.zok
    test/Code/Zokrates/stdlib/hashes/sha256/1024bitPadded.zok
    test/Code/Zokrates/stdlib/hashes/sha256/1024bit.zok
    test/Code/Zokrates/stdlib/hashes/sha256/1536bit.zok
    test/Code/Zokrates/stdlib/hashes/sha256/256bitPadded.zok
    test/Code/Zokrates/stdlib/hashes/sha256/512bitPacked.zok
    test/Code/Zokrates/stdlib/hashes/sha256/512bitPadded.zok
    test/Code/Zokrates/stdlib/hashes/sha256/512bit.zok
    test/Code/Zokrates/stdlib/hashes/sha256/shaRound.zok
    test/Code/Zokrates/stdlib/hashes/utils/256bitsDirectionHelper.zok
    test/Code/Zokrates/stdlib/signatures/verifyEddsa.zok
    test/Code/Zokrates/stdlib/utils/casts/1024to256array.zok
    test/Code/Zokrates/stdlib/utils/casts/bool_128_to_u32_4.zok
    test/Code/Zokrates/stdlib/utils/casts/bool_256_to_u32_8.zok
    test/Code/Zokrates/stdlib/utils/casts/u32_4_to_bool_128.zok
    test/Code/Zokrates/stdlib/utils/casts/u32_8_to_bool_256.zok
    test/Code/Zokrates/stdlib/utils/multiplexer/lookup1bit.zok
    test/Code/Zokrates/stdlib/utils/multiplexer/lookup2bit.zok
    test/Code/Zokrates/stdlib/utils/multiplexer/lookup3bitSigned.zok
    test/Code/Zokrates/stdlib/utils/pack/bool/nonStrictUnpack256.zok
    test/Code/Zokrates/stdlib/utils/pack/bool/pack128.zok
    test/Code/Zokrates/stdlib/utils/pack/bool/pack256.zok
    test/Code/Zokrates/stdlib/utils/pack/bool/unpack128.zok
    test/Code/Zokrates/stdlib/utils/pack/u32/nonStrictUnpack256.zok
    test/Code/Zokrates/stdlib/utils/pack/u32/pack128.zok
    test/Code/Zokrates/stdlib/utils/pack/u32/pack256.zok
    test/Code/Zokrates/stdlib/utils/pack/u32/unpack128.zok
);
typeset -A envvars
envvars=(
         )
init_results
for i in $(seq ${#benches})
    do
    p=$benches[$i]
    echo $p
    for compiler in circify zokrates; do
        count $p $compiler
    done
done
commit_results

