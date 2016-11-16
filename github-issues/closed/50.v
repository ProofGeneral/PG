(*  
    Use proof-goto-point to go to the end

    Right after everything is colored light blue, insert a space right
    before constructor

    Try to move the cursor around and note that it's slow.
*)

Require Coq.ZArith.BinInt.
Ltac sleep := idtac;
              let sleep := eval vm_compute in Coq.ZArith.BinInt.Z.div_eucl in idtac.
Ltac big := do 100 sleep;
            do 1000 idtac "foo".
Theorem foo : True.
Proof.
  idtac. idtac.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  big. big. big. big. big. big. big. big. big. big. big. big. big. big. big.
  constructor.
Qed.

Check True.
Check True.
Check True.
Check True.
