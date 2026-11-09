(*
 * Coq sources for coq-test-silent-option.el
 *
 * The test processes this file with coq-run-completely-silent
 * disabled and checks that goals are shown with Rocq >= 9.2.
 *)

Definition silent_test_def := 1.

Lemma silent_test_lemma : True /\ True.
Proof.
  split.
  (* end of the region asserted in the test *)
  - exact I.
  - exact I.
Qed.
