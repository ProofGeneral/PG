;; This file is part of Proof General.
;; 
;; © Copyright 2021  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Test that the Proof General prelude is correct.
;;
;; I use Proof General prelude here to refer to the initialization
;; commands that Proof General sends to Coq before the first command
;; of the script.

;; This test shows different behaviour before 8.10. When the problem
;; is fixed, I expect that the version distinction is no longer
;; necessary and can be deleted.
;;
;; Load stuff for `coq--version<'
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)

(defconst coq--post-v810 (coq--post-v810)
  "t if Coq is more recent than 8.9")

(message "prelude tests run with Coq version %s; post-v810: %s"
         (coq-version t) coq--post-v810)


;;; Coq source code for tests 

(defconst coq-src-proof
  "Check 1."
  "Coq source code for checking the prelude.")


;;; utility functions

(defun record-buffer-content (buf)
  "Record buffer content of BUF via `message' for debugging.
BUF should be a string."
  (with-current-buffer buf
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (message "%s buffer contains %d chars: %s" buf (length content) content))))

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    ;; (message "wait for coq/compilation with %d items queued\n"
    ;;          (length proof-action-list))
    ;;
    ;; accept-process-output without timeout returns rather quickly,
    ;; apparently most times without process output or any other event
    ;; to process.
    (accept-process-output nil 0.1)))


;;; define the test

(ert-deftest prelude-correct ()
  :expected-result (if coq--post-v810 :passed :failed)
  "Test that the Proof Genneral prelude is correct.
Check that all the commands that Proof General sends as
initialization before the first script command to Coq do not
yield an error."
  (message "prelude-correct test: Check the Proof General prelude")
  (setq proof-three-window-enable nil)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src-proof)
          (proof-goto-point)
          (wait-for-coq)
          (record-buffer-content "*coq*")

          ;; check that there is no error in *coq*
          (with-current-buffer "*coq*"
            (goto-char (point-min))
            (should (not (re-search-forward "Error:" nil t)))))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))
