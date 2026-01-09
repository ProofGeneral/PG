;;; coq-test-proof-stat.el --- Tests for proof-check-report and proof-check-annotate
;;
;; This file is part of Proof General.
;; 
;; © Copyright 2024  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Tests for proof-check-report and proof-check-annotate.

;;; Code:

(defun reset-coq ()
  "Reset Coq and Proof General.
Do `proof-shell-exit' to kill Coq and reset the locked region and
a lot of other internal state of Proof General.  Used at the
beginning of the test when several tests work on the same Coq
source file."
  (when (and (boundp 'proof-shell-buffer)
             (buffer-live-p proof-shell-buffer))
    (proof-shell-exit t)
    (message "Coq and Proof General reseted")))


(ert-deftest proof-check-correct-stat ()
  :expected-result :failed
  "Test `proof-check-report' on a file that is correct in non-opaque parts.
Test that the report buffer contains the expected output."
  (setq proof-three-window-enable nil)
  (reset-coq)
  (find-file "proof_stat.v")

  ;; run check on file where all errors are in opaque parts
  (proof-check-report nil)

  ;; the result buffer should exist
  (should (buffer-live-p (get-buffer "*proof-check-report*")))
  (with-current-buffer "*proof-check-report*"
    ;; the summary should be correct
    (goto-char (point-min))
    (should
     (search-forward "4 opaque proofs recognized: 2 successful 2 FAILING"
                     nil t))
    ;; results for all 3 test lemmas should be correct
    (mapc
     (lambda (s) (should (search-forward s nil t)))
     '("FAIL a1_equal_4"
       "OK   b_equal_6"
       "FAIL b2_equal_6"
       "FAIL use_admit"))))


(ert-deftest proof-check-error-on-error ()
  "Test `proof-check-report' with errors in non-opaque parts.
Check that `proof-check-report' signals an error with the expected message."
  (setq proof-three-window-enable nil)
  (reset-coq)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "proof_stat.v")
          (setq buffer (current-buffer))

          ;; insert an error in non-opaque part
          (goto-char (point-min))
          (should (search-forward "automatic test marker 1" nil t))
          (end-of-line)
          (insert " X ")

          ;; proof-check-report should abort now with an error
          (condition-case err-desc
              (progn
                (proof-check-report nil)
                ;; Still here? Make test fail!
                (should nil))
            (error
             (should
              (equal (error-message-string err-desc)
                     "Error encountered outside opaque proofs after line 10")))))

      ;; clean-up modified file in any case
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(provide 'coq-test-proof-stat)

;;; coq-test-proof-stat.el ends here
