;;; coq-local-vars.el --- Obsolete, see coq-project.el  -*- lexical-binding: t -*-

;; This file is part of Proof General.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Compatibility file.  The code of coq-local-vars.el is in coq-project.el
;; since Proof General 4.6: local variable list tools for coq.
;; This file will be removed, require coq-project instead.

;;; Code:

(require 'coq-project)

(message "coq-local-vars is obsolete, its code is in coq-project")

(provide 'coq-local-vars)

;;; coq-local-vars.el ends here
