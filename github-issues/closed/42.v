Lemma srP P (H : semiReflect P) : reflect P (sr_true H).

  About id.

  Inductive semiReflect (P : Prop) : Set :=
  | PTrue  :   P -> semiReflect P
  | PFalse : ~ P -> semiReflect P.

  Lemma srP P (H : semiReflect P) : reflect P (sr_true H).
  (* Go to point there *)
