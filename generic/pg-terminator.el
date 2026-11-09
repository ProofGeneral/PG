;;; pg-terminator.el --- Electric and double-hit terminators  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 2017-2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The electric terminator processes the script at each hit of its
;; terminator.  The double-hit terminator processes it when the
;; terminator is hit twice quickly; the second hit is not inserted.
;; Enabling one disables the other.
;;
;; `proof-electric-terminator' is the default implementation for the
;; Proof General backend, with `proof-assert-electric-terminator' as
;; its processing function.  It gives every proof assistant using that
;; backend both behaviours.  Another backend can call
;; `proof-double-hit-insert' with its own processing function.

;;; Code:

(require 'proof-useropts)               ; `proof-set-value'

(declare-function proof-inside-comment "proof-syntax")
(declare-function proof-inside-string "proof-syntax")
(declare-function proof-assert-electric-terminator "proof-script")
(defvar proof-terminal-string)

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

(defun proof-double-hit-insert (function &optional count terminator)
  "Insert the key typed, or call FUNCTION on a double hit.
A double hit is the same key typed again within
`proof-double-hit-delay', outside comments and strings, when
`proof-double-hit-enable' is non-nil.  The second hit is not inserted,
the first one is replaced by the string TERMINATOR if non-nil, and
FUNCTION is called without argument.  This way, a key other than the
terminator, bound to the same command, gives the terminator on a double
hit.  With COUNT, insert COUNT times the key."
  (if (and proof-double-hit-enable (null count)
           proof-double-hit--time
           (< (float-time (time-since proof-double-hit--time))
              proof-double-hit-delay)
           (eq (char-before) last-command-event)
           (not (nth 8 (syntax-ppss))))
      (progn
        (setq proof-double-hit--time nil)
        (when terminator
          (delete-char -1)
          (insert terminator))
        (funcall function))
    (self-insert-command (prefix-numeric-value count))
    (setq proof-double-hit--time (and (null count) (current-time)))))

(defun proof-electric-terminator (&optional count)
  "Insert a terminator, maybe sending the command to the assistant.
If we are inside a comment or string, insert the terminator.
Otherwise, if `proof-electric-terminator-enable' is non-nil, process
the command immediately.  If `proof-double-hit-enable' is non-nil,
process it on a double hit instead.

To side-step either action and possibly insert multiple characters,
pass a non-nil COUNT argument, for example M-3 <terminator>."
  (interactive "P")
  (cond
   ;; Keep the former electric-terminator behaviour as the priority if
   ;; both mutually exclusive options somehow become non-nil.
   ((and (not count)
         proof-electric-terminator-enable
         (not (proof-inside-comment (point)))
         (not (proof-inside-string (point))))
    (proof-assert-electric-terminator))
   ((and (not proof-electric-terminator-enable)
         proof-double-hit-enable)
    (proof-double-hit-insert #'proof-assert-electric-terminator count
                             proof-terminal-string))
   (t
    (self-insert-command (prefix-numeric-value count)))))

(provide 'pg-terminator)

;;; pg-terminator.el ends here
