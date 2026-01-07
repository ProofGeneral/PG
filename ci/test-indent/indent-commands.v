
Require Export Coq.Lists.List.

Module Mod.

  Module Foo.
    Axiom X:Set.
    Axiom Y:Set.
    Axiom Z:Set.
  End Foo.
  
  Section Sec.
    Axiom X:Set.
    Axiom Y:Set.
    Axiom Z:Set.
  End Sec.

  Definition a1 := 1.
  Definition A2 := 1.
  Definition arith1:=
    1.
  Definition arith1
    :=
    1.
  Definition
    arith1
    :=
    1.

  Let x := 1.  Let y := 2.
               
  Notation "[ a ; .. ; b ]" := (a :: .. (b :: []) ..) : list_scope.
  
  
  Inductive test : nat -> Prop :=
    C1 : forall n, test n
  | C2 : forall n, test n
  | C3 : forall n, test n
  | C4 : forall n, test n.
  
  Inductive testbar' : nat -> Prop :=
  | Cbar1 : forall n, test n
  | Cbar2 : forall n, test n
  | Cbar3 : forall n, test n
  | Cbar4 : forall n, test n.
  
  Inductive test2 : nat -> Prop
    := | C21 : forall n, test2 n
  | C22 : forall n, test2 n
  | C23 : forall n, test2 n
  | C24 : forall n, test2 n.
  
  Inductive test' : nat -> Prop :=
    C1' : forall n, test' n
  | C2' : forall n, test' n
  | C3' : forall n, test' n
  | C4' : forall n, test' n
  with
    test2' : nat -> Prop :=
    C21' : forall n, test2' n
  | C22' : forall n, test2' n
  | C23' : forall n, test2' n
  | C24' : forall n, test2' n
  with test3' : nat -> Prop :=
    C21' : forall n, test2' n
  | C22' : forall n, test2' n
  | C23' : forall n, test2' n
  | C24' : forall n, test2' n
  with test4' : nat -> Prop :=
  | C21' : forall n, test2' n
  | C22' : forall n, test2' n
  | C23' : forall n, test2' n
  | C24' : forall n, test2' n.
  
  
  Inductive test3
    : nat -> Prop
    := C31 : forall n, test3 n
  | C32 : forall n, test3 n
  | C33 : forall n, test3 n .
  
  (* Goal parenthesizing. *)
  Lemma L : True.
  Proof.
    idtac.
    idtac... (* special case of command ender. *)
    idtac.
  Qed.

  (* Goal starter *)
  Definition Foo:True.
    exact I.
  Defined.

  (* Bullets. *)
  Lemma L : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof.
    idtac. idtac.
    idtac.
    {
      idtac.
      idtac.
    }
    idtac. {
      idtac.
      idtac. }
    { idtac.
      idtac.
    }
    {
      idtac.
      idtac.
    }
    { idtac. {
        idtac.
        idtac.
      }
      idtac.
           
      { idtac . {
          idtac.
          idtac.
        }
      }
      idtac.
    }
    idtac. { idtac. {
        idtac.
        idtac.
      }
    }
    idtac.
    
    { idtac    . {
        idtac.
        idtac.
      }
    }
    {
      idtac.
      idtac.
      idtac. {
        idtac.
        idtac.
      }
      idtac.
    }
  Qed.

  Lemma L : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof.
    idtac.
    { idtac.
      idtac. }
    { 
      idtac.
      idtac.
    }
    { idtac. {
        idtac.
        idtac.
      }
      idtac.
    }
    { idtac. idtac. { idtac.
                      idtac. }
      idtac.
    }
    { idtac. { idtac. {
          idtac.
          idtac.
    }} }
    { idtac. { idtac.
               {
                 idtac.
                 idtac.
               }
               idtac.
               idtac.
               idtac.
      }
      idtac.
    }
    {
      idtac.
      - idtac.
        idtac. {
          idtac.
          idtac.
        }
        idtac.
    }
  Qed.


  Lemma L : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof.
    idtac.
    -
      idtac.
      idtac.
    - idtac.
      idtac.
      + idtac.
        idtac.
      + idtac.
        idtac.
        * idtac.
    - idtac.
  Qed.

  Lemma L : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof.
    idtac.
    -
      idtac.
      idtac.
      idtac.
      idtac.
    - idtac.
      { idtac.
        - idtac.
          idtac.
        - idtac.
          idtac.
      }
    - idtac. idtac. {
        idtac.
        - idtac.
          idtac.
        - idtac.
          idtac.
      }
      idtac.
    - idtac.
  Qed.


  (* goal selectors. *)
  Lemma L : forall x:nat , Nat.iter x (A:=nat) (plus 2) 0 >= x.
  Proof.
    idtac.
    2:idtac.
    3-6:idtac.

    2:{ idtac.
        idtac. }
    { 
      idtac.
      idtac.
    }
    [foo.bar]:{
      idtac.
      - idtac.
        idtac. {
          idtac.
          idtac.
        }
        idtac.
    }
    [foo]:{
      idtac.
      - idtac.
        idtac. {
          idtac.
          idtac.
        }
        idtac.
    }
  Qed.



End Mod.
