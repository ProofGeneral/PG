(* Needs Equations plugin to work. *)

From Coq Require Import Arith Omega Program.
From Equations Require Import Equations.
Require Import Bvector.


Module Equations.
  Equations neg (b : bool) : bool :=
    neg true := false ;
    neg false := true.

  Equations neg' (b : bool) : bool :=
    neg' true
    :=
      false ;
    neg' false :=
      true.

  Lemma neg_inv : forall b, neg (neg b) = b.
  Proof.
    intros b.
    funelim (neg b); now simp neg.
  Defined.


  Inductive list {A} : Type := nil : list | cons : A -> list -> list.

  Arguments list : clear implicits.
  Notation "x :: l" := (cons x l). 

  Equations tail {A} (l : list A) : list A :=
  | nil := nil ;
  | (cons a v) := v.

  Equations tail' {A} (l : list A) : list A :=
  | nil :=
      nil ;
  | (cons a v)
      := v.

  (* The cases inside { } are recognized as record fields, which from
     an indentation perspective is OK *)
  Equations filter {A} (l : list A) (p : A -> bool) : list A :=
    filter nil p := nil; 
    filter (cons a l) p with p a => { 
      | true := a :: filter l p ;
      | false := filter l p
      }.

  Equations filter_ {A} (l : list A) (p : A -> bool) : list A :=
    filter nil p := nil; 
    filter (cons a l) p with p a => { 
      | true :=
          a :: filter l p ;
      | false
        :=
          filter l p
      }.

  Equations filter' {A} (l : list A) (p : A -> bool) : list A :=
    filter' (cons a l) p with p a =>
      { 
      | true := a :: filter' l p ;
      | false := filter' l p
      };
    filter' nil p := nil.

  Equations filter' {A} (l : list A) (p : A -> bool) : list A :=
    filter' (cons a l) p with p a =>
      { 
      | true :=
          a :: filter' l p ;
      | false
        := filter' l p
      };
    filter' nil p := nil.

  Equations filter' {A} (l : list A) (p : A -> bool) : list A :=
    filter' (cons a l) p with p a =>
      { 
        true := a :: filter' l p ;
        false := filter' l p
      };
    filter' nil p := nil.

  Equations filter' {A} (l : list A) (p : A -> bool) : list A :=
    filter' (cons a l) p with p a =>
      { 
        true :=
          a :: filter' l p ;
        false
        := filter' l p
      };
    filter' nil p := nil.

  Equations filter'' {A} (l : list A) (p : A -> bool) : list A :=
    filter'' (cons a l) p with p a =>
      { | true := a :: filter'' l p ;
      | false := filter'' l p };
    filter'' nil p := nil.

  Equations unzip {A B} (l : list (A * B)) : list A * list B :=
    unzip nil := (nil, nil) ;
    unzip (cons p l) with unzip l => {
        unzip (cons (pair a b) l) (pair la lb) := (a :: la, b :: lb) }.


  Equations equal (n m : nat) : { n = m } + { n <> m } :=
    equal O O := left eq_refl ;
    equal (S n) (S m) with equal n m :=
      { equal (S n) (S ?(n)) (left eq_refl) := left eq_refl ;
        equal (S n) (S m) (right p) := right _ } ;
    equal x y := right _.
  
End Equations.
