Definition Foo := nat.
Definition Bar := nat.
(*init*)
From elpi Require Import elpi.


Lemma le_prop: forall n m p:nat, le n m -> le m p -> le n p.
Proof.
  intros n m p. 
  intro H.
  revert p.
  induction H;intros.
  - assumption.
  - { + apply IHle.
        apply le_S_n.
        apply le_S.
        assumption. }
Qed.

(* named goals are in rocq >= 9.2,  *)
Open Scope bool_scope.
Set Generate Goal Names.
Goal forall a b, a && b = b && a.
Proof.
  intros a b. 
  destruct a,b.
  [true.true]: {
    cbn. reflexivity. }
  [true.false]: {
    cbn. reflexivity.
  }
Admitted.

Elpi Tactic show.
Elpi Accumulate lp:{{

  solve (goal Ctx _Trigger Type Proof _) _ :-
    coq.say "Goal:" Ctx "|-" Proof ":" Type.

}}.
