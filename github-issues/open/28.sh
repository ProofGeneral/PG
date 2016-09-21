#!/bin/bash
mkdir -p tmp
cd tmp
cat > A.v <<EOF
Definition x := true.
EOF
cat > B.v <<EOF
Require Import Top.A.
Definition y := x.
EOF
cat > C.v <<EOF
Require Import Top.B.
Require Import Coq.Init.Datatypes.
Definition z := 1.
Section foo.
End foo.
EOF
${COQBIN}coqc -q -Q . "Top" A.v || exit 1
${COQBIN}coqc -q -Q . "Top" B.v || exit 1
cat > A.v <<EOF
Definition x := false.
EOF
${COQBIN}coqc -q -Q . "Top" A.v || exit 1
