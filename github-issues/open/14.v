(* https://github.com/psteckler/ProofGeneral/issues/14 *)

(* modified version of test-suite/interactive/ParalITP.v *)

Fixpoint fib n := match n with
  | O => 1
  | S m => match m with
    | O => 1
    | S o => fib o + fib m end end.

Let time := 1.

Ltac sleep n :=
  try (cut (fib n = S (fib n)); reflexivity).

Section Demo.

Variable i : True.

Lemma a : True.
Proof using i.
  sleep time.
  idtac.
  sleep time.
  exact (i i). (* enter spaces here after error found *)
  trivial.
Qed.

Lemma b : True.
Proof using i. 
  sleep time.
  idtac. (* jump here *)
  sleep time.
  exact a.
Qed.

End Demo.

