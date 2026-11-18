(* Constructions that the indentation recognizes by looking back
   before point, see coq-looking-back and coq-looking-back-near. *)

Module Selectors.
  Lemma l : True /\ True /\ True /\ True.
  Proof.
    refine (conj ?[a] (conj ?[b] (conj ?[c] ?[d]))).
    [a]:{ exact I. }
    2:{
      exact I.
    }
    [d]: {
      exact I. }
    1 : { exact I. }
  Qed.
End Selectors.

Module Controls.
  Lemma l : True /\ True.
  Proof.
    split.
    - Time exact I.
    - Timeout 10 exact I.
  Qed.

  Lemma l2 : True.
  Proof.
    Fail exact 0.
    Redirect "indent-looking-back" Check I.
    Time Timeout 10 exact I.
  Qed.
End Controls.

Module Type T.
  Parameter x : nat.
End T.

Module Type T2.
  Declare Module M : T.
End T2.

Module N : T.
  Definition x := 0.
End N.

Module P : T2 with Module M := N.
  Module M := N.
End P.

Module Terms.
  Definition e : nat := ltac:(exact 0).
  Lemma l : True.
  Proof.
    let x := constr:(I) in
    exact x.
  Qed.
End Terms.
