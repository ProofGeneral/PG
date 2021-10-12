Definition trois := 3. (*test-definition*)
Print trois.
Eval compute in 10 * trois * trois.
 


Lemma easy_proof : forall A : Prop, A -> A.
Proof using .
  intros A.
  intros proof_of_A. (*test-insert*)
  exact proof_of_A.
Qed. (*test-lemma*)

Section failSection.
  Local Unset Ltac Backtrace.
  Goal False.
  Proof. (*FailNoTrace*)
    Fail (now auto).
    auto.
  Abort.

  Local Set Ltac Backtrace.
  Goal False. (*FailTrace*) 
    Fail (now auto).
    auto.
  Abort.
End failSection.


Lemma false_proof : forall A B : bool, A = B. 
Proof.
  intros A B.
  destruct A.
  destruct B.
  reflexivity. (*error*)
  reflexivity.
Qed. (*test-lemma2*)
  
