(* https://github.com/psteckler/ProofGeneral/issues/7 *)

(* If I try to interrupt the checking of a proof using ctrl+c ctrl+c, I get an error message "proof process not started".
*)  

Goal nat.
repeat (generalize 0).
