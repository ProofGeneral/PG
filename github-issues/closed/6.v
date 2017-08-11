(* https://github.com/psteckler/ProofGeneral/issues/6 *)

(* If I ctrl+c ctrl+n twice, the repeat gets colored as if it was processed, whereas it is in fact looping. *)

Goal nat.
  repeat (generalize 0).
  Qed.
