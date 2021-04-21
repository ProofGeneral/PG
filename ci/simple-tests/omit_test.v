
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
