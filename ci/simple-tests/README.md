This directory contains a number of more simple tests, that can
all run in the same directory.

# Overview of existing tests for Coq

coq-test-coqtop-unavailable
: open a file with PG when no coqtop is available
coq-test-omit-proofs
: test the omit proofs feature
coq-test-par-job-needs-compilation-quick
: test coq-par-job-needs-compilation-quick by enumerating all
  possible cases
coq-test-prelude-correct
: test that the Proof General prelude is correct
coq-test-goals-present
: test that Proof General shows goals correctly in various
  situations

# Overview of existing tests for qRHL

qrhl-test-input
: tests relating to the qRHL prover


# Important conventions

The Makefile runs all ERT tests in all `coq-test-*.el` and
`qrhl-test-*.el` files for, respectively, the goals `coq-all` and
`qrhl-all`. Therefore, new tests should be written in a file matching
these patterns.

To run all tests in a single file `file.el`, do `make file.success`.
