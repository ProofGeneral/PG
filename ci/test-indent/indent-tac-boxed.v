Module foo.
  Lemma toto:nat.
  Proof.
    {{
        exact 3.
    }}
  Qed.

  Lemma foo: forall n: nat,
             exists m:nat,
               m = n + 1.
  Proof.
    intros n.
    destruct n. {
      exists 1.
      reflexivity. }
    exists (S (S n)).
    simpl.
    rewrite Nat.add_1_r.
    reflexivity.
  Qed.

  Lemma L : forall x:nat,
              Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof with auto with arith.
    intros x.
    
    induction x;simpl;intros...
    
    induction x;
      simpl ;
      simpl ;
      simpl ;
      intros.
  Qed.
  
  Ltac foo:= intros x;
             induction x;
             simpl ;
             intros.
  

  Lemma L :
    forall x:nat ,
      Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof with auto with arith.
    intros x;
      induction x;
      simpl ;
      intros.
    idtacjqslkjd;[
        intros x ;
        induction x ;
        simpl ;
        intros].
  Qed.



  Lemma L' : forall x:nat ,
               Nat.iter x (A:=nat) (plus 2) 0 >= x
  with L'' : forall x:nat ,
               Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof with auto with arith.
    - induction x;simpl;intros...
    - induction x;simpl;intros...
  Qed.

End foo.

Section SET.
  Definition set (T : Type) := Ensemble T.
  
  Require Import Program.
  
  
  Definition eq_n : forall A n (v:Vector.t A n) n',
                      n=n' ->
                      Vector.t A n'.
  Proof.
    intros A n v n' H.
    rewrite <- H.
    assumption.
  Defined.
  
End SET.

Module curlybracesatend.

  
  Lemma foo2: forall n: nat,
              exists m:nat,
                m = n + 1.
  Proof.
    intros n.
    destruct n. {
      exists 1.
      reflexivity. }
    
    exists (S (S n)).
    simpl.
    rewrite Nat.add_1_r.
    reflexivity.
  Qed.
  
  Lemma foo3: forall n: nat,
              forall n: nat,
              forall n: nat,
              forall n: nat, forall n: nat,
              exists m:nat,
                m = n + 1 ->
                m = n + 1 ->
                m = n + 1
                -> m = n + 1
                -> m = n + 1 ->
                m = n + 1.
  Proof.
    intros n. cut (n = n). {
      destruct n. {
        exists 1.
        reflexivity. } {
        exists (S (S n)).
        simpl.
        rewrite Nat.add_1_r.
        reflexivity. }
    }
    idtac.
    reflexivity.
  Qed.

  Lemma foooooooooooooooo3: forall n: nat,
                            forall n: nat, forall n: nat,
                              f x ->
                              g y ->
                              f x -> forall n: nat,
                                     forall n: nat,
                                     forall n: nat,
                                     exists m:nat,
                                       m = n + 1 ->
                                       m = n + 1 ->
                                       m = n + 1
                                       -> m = n + 1
                                       -> m = n + 1 ->
                                       m = n + 1 -> 
                                       True.
  Proof.
    intros n. cut (n = n).
    {
      destruct n. {
        exists 1.
        reflexivity. } {
        exists (S (S n)).
        simpl.
        rewrite Nat.add_1_r.
        reflexivity.
      }
    }
    idtac.
    reflexivity.
  Qed.

  
End curlybracesatend.

