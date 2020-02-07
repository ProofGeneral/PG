(* Needs ext-lib library to compile. *)

Require Import Coq.ZArith.ZArith_base Coq.Strings.Ascii Coq.Strings.String.
Require Import ExtLib.Data.Monads.StateMonad ExtLib.Structures.Monads.
Import StringSyntax.
Open Scope string_scope.
Section StateGame.
  
  Check Ascii.Space.

  Import MonadNotation.
  Local Open Scope Z_scope.
  Local Open Scope char_scope.
  Local Open Scope monad_scope.

  Definition GameValue : Type := Z.
  Definition GameState : Type := (prod bool Z).

  Variable m : Type -> Type.
  Context {Monad_m: Monad m}.
  Context {State_m: MonadState GameState m}.

  Print Grammar constr.

  Fixpoint playGame (s: string) m' {struct s}: m GameValue :=
    match s with
    |  EmptyString =>
       v <- (if true then m' else get) ;;
       let '(on, score) := v in ret score
    |  String x xs =>
       v <- get ;;
       let '(on, score) := v in
       match x, on with
       | "a"%char, true =>  put (on, score + 1)
       | "b"%char, true => put (on, score - 1)
       | "c"%char, _ =>   put (negb on, score)
       |  _, _  =>    put (on, score)
       end ;;
       playGame xs m'
    end.

  Definition startState: GameState := (false, 0).

End StateGame.

(* Needs Equations plugin to work. *)

From Coq Require Import Arith Omega Program.
From Equations Require Import Equations.
Require Import Bvector.


Module Equations.
  Equations neg (b : bool) : bool :=
    neg true := false ;
    neg false := true.

  Lemma neg_inv : forall b, neg (neg b) = b.
  Proof.
    intros b.
    funelim (neg b); now simp neg.
  Defined.


  Inductive list {A} : Type := nil : list | cons : A -> list -> list.

  Arguments list : clear implicits.
  Notation "x :: l" := (cons x l). 

  Equations tail {A} (l : list A) : list A :=
  | nil :=
      nil ;
  | (cons a v)
      := v.

  (* The cases inside { } are recognized as record fields, which from
     an indentation perspective is OK *)
  Equations filter {A} (l : list A) (p : A -> bool) : list A :=
    filter (cons a l) p with p a =>
    { 
                | true := a :: filter l p ;
    | false := filter l p
    };
    filter nil p := nil 
  .

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
