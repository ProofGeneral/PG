
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

  Definition arith1 a
    (b:nat) c
    d e
    :=
    1.
  Definition
    arith1
      (b:nat) c
      d e
    :=
    1.

  Let x := 1.  Let y := 2.

  Notation "[ a ; .. ; b ]" := (a :: .. (b :: []) ..) : list_scope.
  
  
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
