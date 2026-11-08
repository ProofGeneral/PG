;;; coq-hyps.el --- Obsolete, see coq-goals.el  -*- lexical-binding: t -*-

;; This file is part of Proof General.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Compatibility file.  The code of coq-hyps.el is in coq-goals.el
;; since Proof General 4.6: Folding and highlighting of the hypotheses.
;; This file will be removed, require coq-goals instead.

;;; Code:

(require 'coq-goals)

(message "coq-hyps is obsolete, its code is in coq-goals")

(provide 'coq-hyps)

;;; coq-hyps.el ends here
