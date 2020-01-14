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
