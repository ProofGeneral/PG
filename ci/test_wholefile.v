(* taken from https://coq.inria.fr/distrib/8.2/contribs/QArithSternBrocot.sqrt2.html *)
(* Note: this file contains no "Proof" command (invariant to preserve)
   in order to exercise 070_coq-test-regression-wholefile-no-proof. *)

Require Export ArithRing.
Require Export Compare_dec.
Require Export Wf_nat.
Require Export Arith.
Require Export Lia.

Theorem minus_minus : forall a b c : nat, a - b - c = a - (b + c).
intros a; elim a; auto.
intros n' Hrec b; case b; auto.
Qed.

Remark expand_mult2 : forall x : nat, 2 * x = x + x.
intros x; ring.
Qed.

Theorem lt_neq : forall x y : nat, x < y -> x <> y.
unfold not in |- *; intros x y H H1; elim (lt_irrefl x);
 pattern x at 2 in |- *; rewrite H1; auto.
Qed.
Hint Resolve lt_neq.

Theorem monotonic_inverse :
 forall f : nat -> nat,
 (forall x y : nat, x < y -> f x < f y) ->
 forall x y : nat, f x < f y -> x < y.
intros f Hmon x y Hlt; case (le_gt_dec y x); auto.
intros Hle; elim (le_lt_or_eq _ _ Hle).
intros Hlt'; elim (lt_asym _ _ Hlt); apply Hmon; auto.
intros Heq; elim (lt_neq _ _ Hlt); rewrite Heq; auto.
Qed.

Theorem mult_lt : forall a b c : nat, c <> 0 -> a < b -> a * c < b * c.
intros a b c; elim c.
intros H; elim H; auto.
intros c'; case c'.
intros; lia.
intros c'' Hrec Hneq Hlt;
 repeat rewrite <- (fun x : nat => mult_n_Sm x (S c'')).
lia.
Qed.

Remark add_sub_square_identity :
 forall a b : nat,
 (b + a - b) * (b + a - b) = (b + a) * (b + a) + b * b - 2 * ((b + a) * b).
intros a b; rewrite minus_plus.
repeat rewrite mult_plus_distr_r || rewrite <- (mult_comm (b + a)).
replace (b * b + a * b + (b * a + a * a) + b * b) with
 (b * b + a * b + (b * b + a * b + a * a)); try (ring; fail).
rewrite expand_mult2; repeat rewrite minus_plus; auto with *.
Qed.

Theorem sub_square_identity :
 forall a b : nat, b <= a -> (a - b) * (a - b) = a * a + b * b - 2 * (a * b).
intros a b H; rewrite (le_plus_minus b a H); apply add_sub_square_identity.
Qed.

Theorem square_monotonic : forall x y : nat, x < y -> x * x < y * y.
intros x; case x.
intros y; case y; simpl in |- *; auto with *.
intros x' y Hlt; apply lt_trans with (S x' * y).
rewrite (mult_comm (S x') y); apply mult_lt; auto.
apply mult_lt; lia.
Qed.

Theorem root_monotonic : forall x y : nat, x * x < y * y -> x < y.
exact (monotonic_inverse (fun x : nat => x * x) square_monotonic).
Qed.

Remark square_recompose : forall x y : nat, x * y * (x * y) = x * x * (y * y).
intros; ring.
Qed.

Remark mult2_recompose : forall x y : nat, x * (2 * y) = x * 2 * y.
intros; ring.
Qed.
Section sqrt2_decrease.
Variables (p q : nat) (pos_q : 0 < q) (hyp_sqrt : p * p = 2 * (q * q)).

Theorem sqrt_q_non_zero : 0 <> q * q.
generalize pos_q; case q.
intros H; elim (lt_n_O 0); auto.
intros n H.
simpl in |- *; discriminate.
Qed.
Hint Resolve sqrt_q_non_zero.

Ltac solve_comparison :=
  apply root_monotonic; repeat rewrite square_recompose; rewrite hyp_sqrt;
   rewrite mult2_recompose; apply mult_lt; auto with arith.

Theorem comparison1 : q < p.
replace q with (1 * q); try ring.
replace p with (1 * p); try ring.
solve_comparison.
Qed.

Theorem comparison2 : 2 * p < 3 * q.
solve_comparison.
Qed.

Theorem comparison3 : 4 * q < 3 * p.
solve_comparison.
Qed.
Hint Resolve comparison1 comparison2 comparison3: arith.

Theorem comparison4 : 3 * q - 2 * p < q.
apply plus_lt_reg_l with (2 * p).
rewrite <- le_plus_minus; try (simple apply lt_le_weak; auto with arith).
replace (3 * q) with (2 * q + q); try ring.
apply plus_lt_le_compat; auto.
repeat rewrite (mult_comm 2); apply mult_lt; auto with arith.
Qed.

Remark mult_minus_distr_l : forall a b c : nat, a * (b - c) = a * b - a * c.
intros a b c; repeat rewrite (mult_comm a); apply mult_minus_distr_r.
Qed.

Remark minus_eq_decompose :
 forall a b c d : nat, a = b -> c = d -> a - c = b - d.
intros a b c d H H0; rewrite H; rewrite H0; auto.
Qed.

Theorem new_equality :
 (3 * p - 4 * q) * (3 * p - 4 * q) = 2 * ((3 * q - 2 * p) * (3 * q - 2 * p)).
repeat rewrite sub_square_identity; auto with arith.
repeat rewrite square_recompose; rewrite mult_minus_distr_l.
apply minus_eq_decompose; try rewrite hyp_sqrt; ring.
Qed.
End sqrt2_decrease.
Hint Resolve lt_le_weak comparison2: sqrt.

Theorem sqrt2_not_rational :
 forall p q : nat, q <> 0 -> p * p = 2 * (q * q) -> False.
intros p q; generalize p; clear p; elim q using (well_founded_ind lt_wf).
clear q; intros q Hrec p Hneq; generalize (neq_O_lt _ (sym_not_equal Hneq));
 intros Hlt_O_q Heq.
apply (Hrec (3 * q - 2 * p) (comparison4 _ _ Hlt_O_q Heq) (3 * p - 4 * q)).
apply sym_not_equal; apply lt_neq; apply plus_lt_reg_l with (2 * p);
 rewrite <- plus_n_O; rewrite <- le_plus_minus; auto with *.
apply new_equality; auto.
Qed.
