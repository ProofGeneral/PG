Lemma false_proof : forall A B : bool, A = B. 
Proof.
  intros A B.
  destruct A.
  destruct B.
  reflexivity. (*error*)
  }
Qed. (*test-lemma*)
  
