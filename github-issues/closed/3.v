(* https://github.com/psteckler/ProofGeneral/issues/3 *)

(*
 if I jump after the second Require Import, I get in the log:

 Anomaly: Not yet implemented, the GUI should not try this. Please report.</>

 This is because the state id sent for the second Add is the same as the first.
*) 
 
Require Import List NArith.
Require Import ZArith.
 
