Definition trois := 3. 
Print trois.
Eval compute in 10 * trois * trois.



Lemma  easy_proof : (forall A : Prop, A -> A). 
Proof using .
  intros A.
  intros proof_of_A.
  exact proof_of_A.
Qed.
