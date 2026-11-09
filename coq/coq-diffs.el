;;; coq-diffs.el --- Obsolete, see coq-goals.el  -*- lexical-binding: t -*-

;; This file is part of Proof General.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Compatibility file.  The code of coq-diffs.el is in coq-goals.el
;; since Proof General 4.6: highlight text marked with XML-like tags for Coq diffs.
;; This file will be removed, require coq-goals instead.

;;; Code:

(require 'coq-goals)

(message "coq-diffs is obsolete, its code is in coq-goals")

(provide 'coq-diffs)

;;; coq-diffs.el ends here
