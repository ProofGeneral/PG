;;; coq-abbrev.el --- Obsolete, see coq.el  -*- lexical-binding: t -*-

;; This file is part of Proof General.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Compatibility file.  The code of coq-abbrev.el is in coq.el since
;; Proof General 4.6: the Coq menu of Proof General.
;; This file will be removed, require coq instead.

;;; Code:

(require 'proof)                        ; as the former coq-abbrev.el, for coq.el
(require 'coq)

(message "coq-abbrev is obsolete, its code is in coq")

(provide 'coq-abbrev)

;;; coq-abbrev.el ends here
