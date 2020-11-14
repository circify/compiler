#!/usr/bin/env zsh

typeset -A components
components=(
    c "../../src/Codegen/C ../../src/Codegen/C.hs"
    circom "../../src/Codegen/Circom"
    z3 "../../src/Targets/SMT/TySmtToZ3.hs"
    r1cs "../../src/IR/R1cs.hs ../../src/IR/R1cs/ ../../src/IR/SMT/ToPf.hs"
    zokrates "../../src/Codegen/Zokrates"
    zokrates-all "../../src/Codegen/Zokrates ../../src/AST/Zokrates.hs ../../src/Parser/Zokrates.hs ../../src/Parser/Zokrates"
)

for c in ${(k)components}
do
    n=$(tokei -o json "${(s: :)components[$c]}" | jq .Haskell.code)
    echo "$c: $n"
done


