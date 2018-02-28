(* https://github.com/psteckler/ProofGeneral/issues/4 *)

(*If I jump after that command, Coq does not complain! Going step by step, it does, though. *) 
  
Check (true : False). 

Goal True.
Goal False.
  
