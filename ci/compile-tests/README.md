This directory contains tests for the parallel background
compilation feature for Coq. The test check that
- files get compiled in the right order, 
- after changes, precisely those files that need recompilation
  are compiled
- files are locked and registered in the right require commands.

Each test comes with a hand-crafted set of Coq source files that
implement a particular dependency tree. Therefore, most of the
tests have a subdirectory on their own.

Tests currently missing:
- unlock checks for ancestors of failed jobs in different cases
- a job depending on a failed dependee, where the dependee has
  been finished before
- coq-par-create-file-job detects a dependency cycle
- coq-par-create-file-job finds a job in state waiting-dep
- coq-par-kickoff-queue-maybe is done when the queue dependee is
  in state waiting-queue
- coq-par-create-file-job finds a failed job
- all tests in all quick and all vos variants
