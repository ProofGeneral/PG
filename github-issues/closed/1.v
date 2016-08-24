(* https://github.com/psteckler/ProofGeneral/issues/1 *)

(*
 Steps to reproduce: load a .v file, play one step (ctrl+c ctrl+n), 
 then try to restart the coq process (ctrl+c ctrl+x).
*)
 
Theorem truth : True.
Proof.
  trivial.
Qed.  
