Require Import mathcomp.ssreflect.ssrbool.

Definition xx := nat.
Module foo.
  (* from PG issue #757 *)
  Lemma test:
    forall a : nat,
      a \in [::] ->  (* "\in" should be detected as one token *)
      False.
  Proof.
  Abort.
  Qed.

  Lemma test2:
    forall a : nat,
      a \in  (* "\in" should be detected as one token *)
        [::] ->
      False.
  Proof.
  Abort.
  Qed.
End foo.

