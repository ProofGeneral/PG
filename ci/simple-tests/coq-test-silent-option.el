;;; coq-test-silent-option.el --- Test coq-run-completely-silent with Rocq >= 9.2  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Rocq >= 9.2 does not print goals any more, therefore Proof General
;; must run it completely silent and send Show commands itself.  Test
;; that `coq-run-completely-silent' is ignored (with a warning) for
;; Rocq >= 9.2.  These tests are skipped for older versions.

;;; Code:

(require 'ert)
(defvar coq-auto-adapt-printing-width)
(defvar coq-run-completely-silent)

;; Some Emacs versions run with a frame width of 10 in batch mode
;; inside the container. The line breaks make some regular expressions
;; fail, therfore disable printing width adapting.
(setq coq-auto-adapt-printing-width nil)

;; Set before coq.el is loaded, such that `coq-pg-setup' sees it.
(setq coq-run-completely-silent nil)

(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)
(require 'proof-shell)

(declare-function proof-goto-point "pg-user")
(declare-function proof-unprocessed-begin "proof-script")

(message "silent option tests run with Coq version %s" (coq-version t))


;;; utility functions

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    (accept-process-output nil 0.1)))

(defun silent-option-goto (regexp)
  "Process or retract the current buffer up to the end of REGEXP."
  (goto-char (point-min))
  (should (re-search-forward regexp nil t))
  (proof-goto-point)
  (wait-for-coq)
  (should (equal (proof-unprocessed-begin) (point))))

(defun silent-option-goals ()
  "Return the content of the goals buffer."
  (with-current-buffer proof-goals-buffer
    (buffer-substring-no-properties (point-min) (point-max))))


;;; tests

(ert-deftest silent-option-ignored-for-rocq-9.2 ()
  "`coq-run-completely-silent' nil is ignored for Rocq >= 9.2."
  (skip-unless (coq--post-v92))
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "silent_option.v")
          (setq buffer (current-buffer))
          ;; assert inside the proof: goals must be shown
          (silent-option-goto "split\\.")
          (should (string-match-p "True" (silent-option-goals)))
          ;; retract inside the proof: goals must be shown again
          (silent-option-goto "Proof\\.")
          (should (string-match-p "True /\\\\ True" (silent-option-goals)))
          (with-current-buffer proof-shell-buffer
            (let ((shell (buffer-substring-no-properties
                          (point-min) (point-max))))
              (should-not (string-match-p "Set Silent\\." shell))
              (should (string-match-p "Show\\." shell))))
          (should (get-buffer "*Warnings*"))
          (with-current-buffer "*Warnings*"
            (should (string-match-p
                     "coq-run-completely-silent.* is ignored for Rocq >= 9\\.2"
                     (buffer-string)))))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(provide 'coq-test-silent-option)

;;; coq-test-silent-option.el ends here
