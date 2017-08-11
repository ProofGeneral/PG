(* https://github.com/psteckler/ProofGeneral/issues/15 *)

(* hypothesis / goal separator is too long *)

Goal True.
Proof.
  pose (x := fun x => fun y => x + y).
  pose (y := (x,x)).
  unfold x in y.
