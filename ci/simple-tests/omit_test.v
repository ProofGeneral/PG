(*
 * Coq sources for test-omit-proofs.el
 * 
 * Up to test marker 4 the sources are used for
 * omit-proofs-omit-and-not-omit: The proof of the first lemma
 * classic_excluded_middle should not be omitted, while the proof of the
 * second classic_excluded_middle is.
 *
 * Lemma never_omit_hints is for test omit-proofs-never-omit-hints: Proofs
 * containing commands should never be skipped (except for a few white-listed
 * commands.
 *
 * Lemma never_omit_let is for test omit-proofs-never-omit-lets: Proofs of
 * let-theorems should never be omitted.
 * 
 *)


Definition classical_logic : Prop := forall(P : Prop), ~~P -> P.

(* automatic test marker 1 *)

Lemma classic_excluded_middle :
  (forall(P : Prop), P \/ ~ P) -> classical_logic.
Proof.
  intros H P H0.
  (* automatic test marker 2 *)
  specialize (H P).
Abort.

Lemma classic_excluded_middle :
  (forall(P : Prop), P \/ ~ P) -> classical_logic.
Proof using.
  intros H P H0.
  specialize (H P).
  (* automatic test marker 3 *)
  destruct H.
    trivial.
  contradiction.
Qed.

(* automatic test marker 4 *)

Section let_test.

  Let never_omit_let : 1 + 1 = 2.
  (* some comment between let and proof start *)
  Proof.
    (* automatic test marker 7-1 *)
    auto.
  Qed.

  Let two : nat := 2.
  Lemma behind_let : 1 + 1 = 2.
  Proof using.
    (* automatic test marker 7-2 *)
    auto.
  Qed.

End let_test.

(* automatic test marker 8 *)

Lemma never_omit_hints : 1 + 1 = 2.
Proof using.
  (* Note that attributes such as #[local] were only introduced in Coq 8.9. *)
  #[local] Hint Resolve classic_excluded_middle : core.
  (* automatic test marker 5 *)
  auto.
Qed.

(* automatic test marker 6 *)
