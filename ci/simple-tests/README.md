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
: Test that Proof General shows goals and responses correctly in
  various situations and that the expected thing is visible in
  two-pane mode. Test also that extending and retracting works as
  expected when huge goals cause a delay.
coq-test-three-window
: Test three-pane mode for different frame sizes, including ones that
  are too small for three windows.
coq-test-proof-stat
: test proof-check-proofs
coq-test-time-commands
: test that `coq-time-commands` prefixes the commands sent to Coq with
  Time, except bullets and internal commands
coq-test-hook-variables
: test that the hooks of proof-shell.el can read and modify the
  variables they share with Proof General through dynamic binding,
  with proof-shell.el interpreted and byte-compiled
coq-test-silent-option
: test that `coq-run-completely-silent` is ignored with a warning for
  Rocq >= 9.2, which does not print goals any more (skipped for older
  versions)
coq-test-double-hit
: test the default double-hit terminator, its Coq use, its exclusion
  with the unchanged electric terminator, and that the terminator
  command is common to all backends
coq-test-queries
: test that the query commands are common to all backends and that
  their printing flags are for one query only
coq-test-without-pg
: test Coq mode without the Proof General backend: no proof shell is
  loaded, the abbrevs and the commands common to all backends are there,
  and the project file sets the load path
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
