
Definition a : nat := 4.

Lemma a1_equal_4 : a = 2 * 2.                               (* FAIL *)
Proof using.
  simpl.
  zzzz.                         (* this proof should fail *)
  trivial.
Qed.

Definition b : nat :=
(* automatic test marker 1 *)
 6.

Lemma b_equal_6 : b = 2 * 3.
Proof using.
  simpl.
  trivial.
Qed.

Lemma b2_equal_6 : b = 2 * 3.                               (* FAIL *)
Proof using.                    (* this proof should fail *)
Qed.

Lemma use_admit : 0 = 1.
Proof using.               (* this proof succeeds but should count as failing *)
  admit.
Admitted.
