This directory contains tests for the parallel background
compilation feature for Coq. The test check that
- files get compiled in the right order, 
- after changes, precisely those files that need recompilation
  are compiled
- files are locked and registered in the right require commands.

Each test comes with a hand-crafted set of Coq source files that
implement a particular dependency tree. Therefore, most of the
tests have a subdirectory on their own.

# Overview of existing tests

All tests are for parallel background compilation.

001-mini-project
: test compilation for a simple project
002-require-no-dependencies
: Test a require that does not produce any dependencies.
003-require-error
: coqdep fails on a require
004-dependency-cycle
: dependency cycle
005-change-recompile
: test that the right files are recompiled when changes are made
006-ready-dependee
: dependency in state ready
007-slow-require
: test almost all internal state combinations with delay on
  coqdep and coqc
008-default-dir
: test that the default/current directory is set correctly
  independent of user/emacs changing the current buffer during
  first and second stage compilation
009-failure-processing
: check ancestor unlocking for a failed job with
  coq-compile-keep-going; test also the case, where the last
  (failed) require job must be delayed, because some queue
  dependee is still processing
010-coqdep-errors
: check that coqdep errors are reliably detected

# Tests currently missing

- a job depending on a failed dependee, where the dependee has
  been finished before
- coq-par-create-file-job detects a dependency cycle
- coq-par-create-file-job finds a job in state waiting-dep
- coq-par-kickoff-queue-maybe is done when the queue dependee is
  in state waiting-queue
- coq-par-create-file-job finds a failed job
- all tests in all quick and all vos variants
- test two coq-par-kickoff-queue-from-action-list entries being
  active at the same time: assert one region, let compilation
  fail, assert second region, while first region is still busy,
  this should fail in some weird way, because failed is not
  propagated
