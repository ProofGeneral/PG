;;; coq-company-compat.el, compatibility file for use with company-coq

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel
;; Portions © Copyright 2016-2018  Massachusetts Institute of Technology

;;; DECLARATIONS

;; variables

(defvar proof-shell-last-goals-output "")
(defvar proof-shell-last-response-output "")

;;; ALIASES

;; variables

(defvaralias 'proof-shell-proof-completed 'proof-prover-proof-completed)

;; functions

(defalias 'proof-shell-invisible-command 'proof-prover-invisible-command)
(defalias 'proof-shell-available-p 'proof-prover-available-p)
(defalias 'proof-shell-live-buffer 'proof-prover-live-buffer)
(defalias 'proof-shell-ready-prover 'coq-response-complete)

(provide 'coq-company-compat)




