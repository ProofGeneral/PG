
Require Export
  Coq.Lists.List.
Require
  Export
    Arith.

Module
  Mod.

  Module
    Foo.
    Axiom
      X
      :
      Set.
    Axiom Y
      :
      Set.
    Axiom Z:
      Set.
  End
    Foo.
  

  Definition a1 :=
    1.
  Definition A2
    := 1.
  Definition arith1
    :=
    1.
  Definition
    arith1
    :=
    1.

  Let x := 1.  Let y := 2.

  Notation "[ a ; .. ; b ]" := (a :: .. (b :: []) ..) : list_scope.
  Definition foo :=
    foo x (y
             a b)
      z t  (* align with function foo + 2. *)
      u v.  (* align with arg z on bol of previous line *)

  
  Inductive test
    : nat 
      -> 
        Prop :=
    C1 : forall n,
           test n
  | C2 : forall n,
           test
             n
  | C3 :
    forall
      n, test n
  | C4 : forall n, test n.
  
  
  (* Goal parenthesizing. *)
  Lemma L :
    True.
  Proof.
    idtac.
    idtac... (* special case of command ender. *)
    idtac.
  Qed.


  Lemma L4 : forall x:nat,
               Nat.iter x (A:=nat)
                 (plus 2) 0 >= x.
  Proof.
    idtac.
  Qed.

  Lemma L3 : forall x:nat,
               Nat.iter
                 x 
                 (A:=nat)
                 (plus 2)
                 0 >= x.
  Proof.
    idtac.
  Qed.

  Lemma L1 : forall x:nat, Nat.iter x 
                             (A:=nat)
                             (plus 2)
                             0 >= x.
  Proof.
    idtac.
  Qed.

  Lemma L2 : forall x:nat, Nat.iter
                             x 
                             (A:=nat)
                             (plus 2)
                             0 >= x.
  Proof.
    idtac.
  Qed.



End Mod.
