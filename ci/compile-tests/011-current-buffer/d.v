(* This file is part of Proof General.
 *
 * © Copyright 2024  Hendrik Tews
 *
 * Authors: Hendrik Tews
 * Maintainer: Hendrik Tews <hendrik@askra.de>
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *
 * This file is part of an automatic test case for parallel background
 * compilation in coq-par-compile.el. See test.el in this directory.
 *)

Definition c : nat := 2.

(* Set `coq-compiler` and `coq-dependency-analyzer` as local variable
   to something that definitely fails when the test (or the user)
   visits this file in an ongoing background compilation and
   background compilation picks up local variables from this file.
 *)

(*** Local Variables: ***)
(*** coq-dependency-analyzer: "coq-error" ***)
(*** End: ***)
