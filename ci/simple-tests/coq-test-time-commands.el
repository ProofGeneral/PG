;;; coq-test-time-commands.el --- Test the Time prefix of coq-time-commands  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; When `coq-time-commands' is non-nil, `coq-preprocessing' (on
;; `proof-shell-insert-hook') prefixes the commands sent to Coq with
;; Time, except bullets and commands sent internally by Proof General.
;; It does so by modifying the variables `string' and `action', which
;; `proof-shell-insert' binds dynamically.  Test that the commands
;; actually sent to Coq are the expected ones.

;;; Code:

(require 'ert)
(defvar coq-auto-adapt-printing-width)
(defvar coq-time-commands)

;; Some Emacs versions run with a frame width of 10 in batch mode
;; inside the container. The line breaks make some regular expressions
;; fail, therfore disable printing width adapting.
(setq coq-auto-adapt-printing-width nil)

(require 'subr-x)
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)

(declare-function proof-process-buffer "pg-user")
(declare-function proof-unprocessed-begin "proof-script")

(message "time commands tests run with Coq version %s" (coq-version t))


;;; utility functions

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    (accept-process-output nil 0.1)))

(defun process-time-commands-file ()
  "Process time_commands.v with `coq-time-commands' enabled.
Return the content of the Coq shell buffer."
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "time_commands.v")
          (setq buffer (current-buffer))
          ;; Set the global value: a `let' here would not work, because
          ;; this file uses lexical binding and `coq-time-commands' is
          ;; not yet known as a dynamic variable when the `let' is
          ;; evaluated (coq.el is only loaded by `find-file').
          (setq coq-time-commands t)
          (proof-process-buffer)
          (wait-for-coq)
          ;; the whole file must have been processed without error
          (goto-char (point-max))
          (skip-chars-backward " \t\n")
          (should (>= (proof-unprocessed-begin) (point)))
          (with-current-buffer proof-shell-buffer
            (buffer-substring-no-properties (point-min) (point-max))))

      ;; clean up
      (setq coq-time-commands nil)
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))


;;; tests

(ert-deftest time-commands-prefix ()
  "Commands are sent with Time, except bullets and internal commands."
  (let ((shell (process-time-commands-file)))
    ;; ordinary commands are timed
    (should (string-match-p "Time Definition time_test_def := 1\\." shell))
    (should (string-match-p "Time Lemma time_test_lemma" shell))
    (should (string-match-p "Time split\\." shell))
    (should (string-match-p "Time exact I\\." shell))
    (should (string-match-p "Time Qed\\." shell))
    ;; Coq does not accept Time before a bullet
    (should-not (string-match-p "Time -" shell))
    ;; the automatic Show sent by Proof General is internal
    (should-not (string-match-p "Time Show" shell))))

(provide 'coq-test-time-commands)

;;; coq-test-time-commands.el ends here
