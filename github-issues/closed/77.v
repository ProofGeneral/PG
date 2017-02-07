(* test case for issue #77 *)

(* Jump to the final "." after the intros. Now hit
 * "backsapce". I would expect PG to undo the intros tactic and move the
 * proof state to before that tactic. Instead, the proof state doesn't
 * change, and "intros" (without the dot) is still marked as processed. 
 *)

Lemma foo : True -> True.
Proof.
  intros.
