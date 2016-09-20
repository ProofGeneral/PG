(* https://github.com/psteckler/ProofGeneral/issues/25 *)

(* 
 "If I go to the end and do proof-goto-point, then the cursor stays at
 the end, and I get no indication that there's an error. (The line
 gets bolded; it should get colored red, and the cursor should jump to
 that point. Or, at least, there should be a setting that makes the
 cursor jump to that point." 

 Paul Steckler notes: this is apparently due to the lack of a
 fail-value corresponding to the error in 8.6. The bug does not appear
 with 8.5pl2.

*)

Definition x := 1.
Section foo.
  Inductive x := a.
End foo.
Section bar.
  Context (y : nat).
  Goal True.
    constructor.
  Qed.
End bar.
