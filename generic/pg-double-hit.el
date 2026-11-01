;;; pg-double-hit.el --- Double hit on the terminator  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 2017-2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Hitting the terminator twice quickly processes the script up to
;; it, the second hit is not inserted.  A replacement for the electric
;; terminator, which processes at each hit.
;;
;; This file depends neither on the prover nor on the proof shell: a
;; command bound to the terminator calls `proof-double-hit-insert'
;; with the function processing up to point.  The Coq backends (Proof
;; General and Eglot) both use it.

;;; Code:

(require 'proof-useropts)               ; `proof-set-value'

(defcustom proof-double-hit-enable nil
  "Whether hitting the terminator twice quickly processes up to it.
The second hit is not inserted.  This replaces the electric
terminator, see `proof-electric-terminator-enable': enabling one
disables the other."
  :type 'boolean
  :set #'proof-set-value
  :group 'proof-user-options)

(defvar proof-double-hit-delay 0.25
  "The maximum delay between the two hits of a double hit, in seconds.")

(defun proof-double-hit-enable ()
  "Disable the electric terminator if the double hit is enabled.
Called by `proof-set-value' on `proof-double-hit-enable'."
  (when (and proof-double-hit-enable
             (bound-and-true-p proof-electric-terminator-enable)
             (fboundp 'proof-electric-terminator-toggle))
    (proof-electric-terminator-toggle 0)))

(defun proof-double-hit-toggle (&optional arg)
  "Toggle `proof-double-hit-enable'.  With ARG, turn on iff ARG>0."
  (interactive "P")
  (customize-set-variable 'proof-double-hit-enable
                          (if (null arg) (not proof-double-hit-enable)
                            (> (prefix-numeric-value arg) 0))))

(defvar proof-double-hit--time nil
  "Time of the last hit, while a double hit is possible.")

(defun proof-double-hit-insert (function &optional count)
  "Insert the key typed, or call FUNCTION on a double hit.
A double hit is the same key typed again within
`proof-double-hit-delay', outside comments and strings, when
`proof-double-hit-enable' is non-nil.  The second hit is not inserted
and FUNCTION is called without argument, after the first one.  With
COUNT, insert COUNT times the key."
  (if (and proof-double-hit-enable (null count)
           proof-double-hit--time
           (< (float-time (time-since proof-double-hit--time))
              proof-double-hit-delay)
           (eq (char-before) last-command-event)
           (not (nth 8 (syntax-ppss))))
      (progn
        (setq proof-double-hit--time nil)
        (funcall function))
    (self-insert-command (prefix-numeric-value count))
    (setq proof-double-hit--time (and (null count) (current-time)))))

(provide 'pg-double-hit)

;;; pg-double-hit.el ends here
