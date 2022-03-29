
Module foo.
  Lemma toto:nat.
  Proof.
    {{
        exact 3.
    }}
  Qed.

  Lemma L : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof with auto with arith.
    intros x.
    
    induction x;simpl;intros...

    induction x;
      simpl ;
      simpl ;
      simpl ;
      intros.

    Ltac foo:=
      intros x;
      induction x;
      simpl ;
      simpl ;
      simpl ;
      intros.

    
    induction x
    ;
      simpl
    ;
      intros
    ;
      intros
    ;
      intros
    .
    
    
    idtac
    ;
      [
        induction x
        ;
          simpl
        ;
          intros
      ].

    
    Ltac foo
      :=
      intros x
      ;
        induction x
      ;
        simpl
      ;
        intros
      ;
        intros
    .
    

    idtac
    ;
      [
        induction x
        ;
          simpl ;
        simpl
      | intros
      ].
    
    Ltac foo :=
      intros x
      ;
        induction x
      ;
        simpl
      ;
        intros
      ;
        intros
    .
    



    induction x ;
      simpl ;
      intros.

    induction x
    ;
      simpl ;
      intros.

    induction x ;
      simpl
    ;
      simpl ;
      intros.
    
    
    intros x
    ; induction x
    ; simpl
    ; intros.
    
    idtac;(intros x
           ; induction x
           ; simpl
           ; intros).
    

    idtac;( induction x;
            simpl ;
            intros ).
    
    idtac;[ intros x
            ; induction x
            ; simpl
            ; intros].

    idtac
    ;
      [
        induction x;
        simpl ;
        intros
      | simpl
        ; intros ].


    idtac;[ intros x
            ; induction x
          | simpl
            ; intros].
    

    idtac;[ intros x ;
            induction x
          | simpl ;
            intros
      ].
    

    idtac foobar;[ induction x;
                   simpl ;
                   intros ].

  Qed.

  Ltac foo:=
    intros x;
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
  Lemma L : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof with auto with arith.
    intros x;
      [ induction x;
        [
          simpl;
          intros...
        ]
      ].
  Qed.


  Lemma L2 : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof with auto with arith.
    idtac.
    (* "as" tactical *)
    induction x
      as
      [ | x IHx]. 
    - cbn.
      apply Nat.le_trans
        with
        (n:=0) (* aligning the different closes of a "with". *)
        (m:=0)
        (p:=0).
      + auto with arith.
      + auto with arith.
    - simpl.
      intros.
      auto with arith.
  Qed.

  Lemma L' : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x
  with L'' : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof with auto with arith.
    - induction x;simpl;intros...
    - induction x;simpl;intros...
  Qed.
  Lemma L''' : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof using Type *.
    intros x.
    induction x;simpl;intros.  
    admit.
  Admitted.

  Lemma L'''' : forall x:nat ,  0 <= x.
  Proof (fun x : nat => Nat.le_0_l x).
  (* no indentation here since the proof above closes the proof. *)
  Definition foo:nat := 0.
End Y.

Function div2 (n : nat) {struct n}: nat :=
  match n with
  | 0 => 0
  | 1 => 0
  | S (S n') => S (div2 n')
  end.


Module M1.
  Module M2.
    Lemma l1: forall n:nat, n = n. 
      auto.
    Qed.
    Lemma l2: forall n:nat, n = n. 
      auto. Qed.
    Lemma l3: forall n:nat, n <= n. auto. Qed.
    (*   Lemma l4: forall n:nat, n <= n. Proof. intro. Qed. *)
    Lemma l5 : forall n:nat, n <= n.
    Proof. auto.
    Qed.
    Lemma l6: forall n:nat, n = n. 
      intros.
      Lemma l7: forall n:nat, n = n.
        destruct n.
        {
          auto.
        }
        {
          destruct n.
          {
            auto.
          }
          auto.
        }
      Qed.
      {
        destruct n.
        {
          auto. }
        { auto.
        }
      }
    Qed.
  End M2.
End M1.

Module GoalSelectors.
  Theorem lt_n_S : (True \/ True \/ True \/ True \/ True ) -> True.
  Proof.
    refine (or_ind ?[aa] (or_ind ?[bb] (or_ind ?[cc] (or_ind ?[dd] ?[ee])))).
    [aa]:{ auto. }
    2:{  auto. }
    [ee]:auto.
    {  auto. }
  Qed.
  (* Same without space between "." and "}". *)
  Theorem lt_n_S2 : (True \/ True \/ True \/ True \/ True ) -> True.
  Proof.
    refine (or_ind ?[aa] (or_ind ?[bb] (or_ind ?[cc] (or_ind ?[dd] ?[ee])))).
    [aa]:{ auto. }
    2:{  auto. }
    [ee]:auto.
    {  auto. }
  Qed.


End GoalSelectors.

Module M1'.
  Module M2'.
    Lemma l6: forall n:nat, n = n.
    Proof.
      intros.
      Lemma l7: forall n:nat, n = n.
      Proof.
        destruct n. intros.
        {
          auto.
        }
        { 
          destruct n. 
          idtac a
          ;
            [
              auto;
              auto
            |
              auto;
              auto.
            ].
        }
        auto.
        } 
      Qed.
      {destruct n.
       {
         auto. }
       {auto. }
      }
    Qed.
  End M2'.
End M1'.

(* TODO: add multichar bullets once coq 8.5 is out *)
Module M4'.
  Module M2'.
    Lemma l6: forall n:nat, n = n. 
    Proof.
      intros.
      Lemma l7: forall n:nat, n = n. 
      Proof.
        destruct n.
        - auto.
        - destruct n.
          + idtac;[
                auto
              ].
          + destruct n.
            * auto.
            * auto.
      Qed.
      {destruct n.
       - auto.
       - auto. 
      }
    Qed.
  End M2'.
End M4'.


Module M1''.
  Module M2''.
    Lemma l7: forall n:nat, n = n. 
      destruct n.
      { auto. }
      { destruct n.
        { idtac; [ auto ]. }
        auto. } 
    Qed.
  End M2''.
End M1''.


Record rec:Set := {
    fld1:nat;
    fld2:nat;
    fld3:bool
  }.

Class cla {X:Set}:Set := {
    cfld1:nat;
    cld2:nat;
    cld3:bool
  }.



Module X.
  Lemma l:
    forall r:rec,
    exists r':rec,
      r'.(fld1) = r.(fld2)/\ r'.(fld2) = r.(fld1).
  Proof.
    idtac.
    idtac.
    idtac.
    intros r. {
      exists
        {|
          fld1:=r.(fld2);
          fld2:=r.(fld1);
          fld3:=false
        |}.
      split.
      {auto. }
      {auto. }
    }
    intros r. {
      exists
        {|
          fld1:=
            r.(fld2);
          fld2
          :=r.(fld1);
          fld3
          :=
            false
        |}.
      split.
      {auto. }
      {auto. }
    }
    auto.
    auto.
  Qed.
  
  (* Issue #574 *)
  Goal let x := 1 in True.
  Proof.
    intro.
    match goal with
    | y := _ : unit |- _ => idtac "unit"
    | y := _ : nat |- _ => idtac "nat"
    end.
  Qed.

  Lemma l2 :
    forall r:rec,
    exists r':rec,
      r.(fld1)
      = r'.(fld2)
      /\ r.(fld2)
         = r'.(fld1).
  Proof.
    intros r.
    {{
        idtac;
          exists
            {|
              fld1:=r.(fld2);
              fld2:=r.(fld1);
              fld3:=false
            |}.
        (* ltac *)
        match goal with
          _:rec |- ?a /\ ?b => split
        | _ => fail
        end.

        match goal with
        | ?g := _:rec |- ?a /\ ?b => split
        | _ => fail
        end.

        Fail
          lazymatch goal with
            _:rec |- ?a /\ ?b => split
          | _ => fail
          end.

        Fail
          multimatch goal with
            _:rec |- ?a /\ ?b => split
          | _ => fail
          end.

        { simpl. auto. }
        { simpl. auto. }}}

    - split.
      match
        goal
      with
        X => foo
      end.
    - split.
      match goal with
        X |- _ => foo
      | H: X := Y |- _ => foo
      end.
    - split.
      match H with
        ?a = ?b |- _ => foo
      | ?a < ?b |- _ => foo
      end.
    - split.
      let x := f y in
      let foo := idtac x in
      idtac foo.
  Qed.
End X.

Require Import Morphisms.
Generalizable All Variables.
Local Open Scope signature_scope.
Require Import RelationClasses.

Module TC.
  Instance: (@RewriteRelation nat) impl.
  (* No goal created *)
  Definition XX := 0.
  
  
  Instance StrictOrder_Asymmetric `(StrictOrder A R) : Asymmetric R.
  (* One goal created. Then the user MUST put "Proof." to help indentation *)
  Proof.
    firstorder.
  Qed.
  
  
  Program Fixpoint f (x:nat) {struct x} : nat :=
    match x with
    | 0 => 0
    | S y => S (f y)
    end.
  
  Program Instance all_iff_morphism {A : Type} :
    Proper (pointwise_relation A iff ==> iff) (@all A).
  
  Next Obligation.
  Proof.
    unfold pointwise_relation, all in *.
    intro.
    intros y H.    
    intuition ; specialize (H x0) ; intuition.
  Qed.
  
End TC.

Require Import Sets.Ensembles.
Require Import Bool.Bvector.

Section SET.
  Definition set (T : Type) := Ensemble T.
  
  Require Import Program.
  
  
  Definition eq_n : forall A n (v:Vector.t A n) n', n=n' -> Vector.t A n'.
  Proof.
    intros A n v n' H.
    rewrite <- H.
    assumption.
  Defined.
  
  Fixpoint setVecProd (T : Type) (n : nat) (v1:Vector.t (set T) n) {struct v1}:
    (Vector.t T n) ->  Prop :=
    match v1 with
      Vector.nil _ =>
        fun v2 =>
          match v2 with 
            Vector.nil _ => True
          | _ => False
          end
    | (Vector.cons _ x n' v1') =>
        fun v2 =>
          (* indentation of dependen "match" clause. *)
          match v2
                as
                X
                in
                Vector.t _ n''
                return
                (Vector.t T (pred n'') -> Prop) -> Prop
          with 
          | Vector.nil _ => fun _ => False 
          | (Vector.cons _ y n'' v2') => fun v2'' => (x y) /\ (v2'' v2')
          end (setVecProd T n' v1')
    end.
  
End SET.

Module curlybracesatend.

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
  
  Lemma foo3:
    forall n: nat,
    forall n: nat,
    forall n: nat,
    forall n: nat,
    forall n: nat,
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

