(*
 * Coq sources for coq-test-time-commands.el
 *
 * The test processes this file with coq-time-commands enabled and
 * checks which commands were sent with the Time prefix.
 *)

Definition time_test_def := 1.

Lemma time_test_lemma : True /\ True.
Proof.
  split.
  - exact I.
  - exact I.
Qed.
