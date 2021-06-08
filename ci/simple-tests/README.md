This directory contains a number of more simple tests, that can
all run in the same directory.

# Overview of existing tests

test-coqtop-unavailable
: open a file with PG when no coqtop is available
test-omit-proofs
: test the omit proofs feature
coq-par-job-needs-compilation-quick
: test coq-par-job-needs-compilation-quick by enumerating all
  possible cases
test-goals-present
: test that Proof General shows goals correctly in various
  situations


# Important conventions

The Makefile runs all ERT tests in all `test-*.el` files.
Therefore, the test should be written in a file matching this
pattern.

To run all tests in a single file, do `make test-*.success`.
