(* https://github.com/psteckler/ProofGeneral/issues/27 *)

(* 

 "If I go to the end and do proof-goto-point, and then I then place the
 cursor before End foo. and do proof-goto-point, then it moves the
 cursor to right after Section foo.. This is incorrect; if I start
 typing XYZ before End, I should not get Section foo.YZ ... XEnd
 foo.. It should all go immediately before End."

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
