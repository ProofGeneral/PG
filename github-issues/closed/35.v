(* https://github.com/psteckler/ProofGeneral/issues/35 *)

(* PG-xml says no more goals when there are more subgoals (with bullets) *)

Goal True /\ True.
Proof.
  split.
   - exact I.
 
