(*
 * Coq sources for coq-test-response-after-show.el
 *
 * Each test processes this file one command at a time up to a marker
 * comment and checks the goals and response buffers.
 *)

#[deprecated(since="1.0", note="use idtac")] Ltac my_tac := idtac.

Lemma a : True /\ True /\ True.
Proof.
  split; [|split].
  - idtac "hello". (* marker hello *)
    my_tac. (* marker warning *)
    Time exact I. (* marker subproof *)
  - admit.
  - Time exact I. (* marker given up *)
Admitted.

Lemma b : True.
Proof.
  Time exact I. (* marker no more goals *)
Qed.
