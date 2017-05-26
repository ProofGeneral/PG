;;; proof-auxmodes.el --- Arrange for auxiliary modes to be loaded when required

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel
;; Portions © Copyright 2012, Hendrik Tews
;; Portions © Copyright 2017, Clément Pit-Claudel
;; Portions © Copyright 2016-2017, Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:
;;
;; Startup code from auxiliary modes are collected here, allowing late
;; loading of their main defining files and the possibility to disable them.
;;

(require 'proof-utils)			; proof-ass, proof-eval...

;;; Code:

;;
;; Maths menu
;;
(defun proof-maths-menu-support-available ()
  "A test to see whether maths-menu support is available.
The test loads optional prover-specific config in <foo>-maths-menu.el"
  (or (proof-try-require (proof-ass-sym maths-menu)) t))

(proof-eval-when-ready-for-assistant
    (if (and (proof-ass maths-menu-enable)
	     (proof-maths-menu-support-available))
	(proof-maths-menu-set-global t)))


;;
;; Unicode tokens
;;
(defun proof-unicode-tokens-support-available ()
  "A test to see whether unicode tokens support is available."
  ;; Requires prover-specific config in <foo>-unicode-tokens.el
  ;; Loaded before unicode-tokens.el to allow load-time config there
  (proof-try-require (proof-ass-sym unicode-tokens)))

(proof-eval-when-ready-for-assistant
    (if (and (proof-ass unicode-tokens-enable)
	     (proof-unicode-tokens-support-available))
	(proof-unicode-tokens-set-global t)))


(provide 'proof-auxmodes)

;;; proof-auxmodes.el ends here
