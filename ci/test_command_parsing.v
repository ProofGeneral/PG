Definition Foo := nat.
Definition Bar := nat.
(*init*)
From elpi Require Import elpi.


Lemma le_prop: forall n m p:entier, le n m -> le m p -> le n p.
Proof.
  intros n m p. 
  intro H.
  revert p.
  induction H;intros.
  - assumption.
  - { + apply IHle.
        apply le_Suuc_a_gauche.
        assumption. }
Qed.



Elpi Tactic show.
Elpi Accumulate lp:{{

  solve (goal Ctx _Trigger Type Proof _) _ :-
    coq.say "Goal:" Ctx "|-" Proof ":" Type.

}}.
