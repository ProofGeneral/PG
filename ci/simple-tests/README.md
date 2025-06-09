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
: Test that Proof General shows goals correctly in various situations.
  Test also that in other situations the response buffer contains the
  right output and is visible in two-pane mode.
coq-test-three-window
: Test three-pane mode for different frame sizes, including ones that
  are too small for three windows.
coq-test-proof-stat
: test proof-check-proofs
coq-proof-stat-batch-test
: Batch mode test for proof-check-proofs. There is no Emacs lisp file
  for this test. It is programmed out in the Makefile goal
  coq-proof-stat-batch-test.

# Overview of existing tests for qRHL

qrhl-test-input
: tests relating to the qRHL prover


# Important conventions

The Makefile runs all ERT tests in all `coq-test-*.el` and
`qrhl-test-*.el` files for, respectively, the goals `coq-all` and
`qrhl-all`. Therefore, new tests should be written in a file matching
these patterns.

To run all tests in a single file `file.el`, do `make file.success`.

To run a single test use 
```
emacs -batch -l ../../generic/proof-site.el -l <file.el> --eval '(ert-run-tests-batch-and-exit "<ert-test-name>")'
```
