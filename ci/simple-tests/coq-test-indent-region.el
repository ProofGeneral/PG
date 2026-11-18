;;; coq-test-indent-region.el --- Test of the token cache of indent-region  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; `coq-smie-indent-region' caches the tokens read backward, see
;; `coq-smie--tokens'.  The indentation must be the same as without
;; the cache, also when each line changes: test it on the files of
;; ci/test-indent, flushed left.  Without backend: the indentation is
;; the same in all of them.

;;; Code:

;; before loading coq-mode.el, which defines it
(defvar coq-use-pg)
(setq coq-use-pg nil)

(require 'proof-site)
(require 'ert)
(require 'cl-lib)
(with-temp-buffer (coq-mode))           ; loads it, through its autoload

(defvar coq-smie--tokens)
(defvar coq-indent-box-style)

(defconst indent-region-test-directory
  (expand-file-name "../test-indent/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The directory of the indentation tests.")

(defun indent-region-test (file cache)
  "Text of FILE flushed left, then indented by `indent-region'.
With the token cache if CACHE is non-nil."
  (with-temp-buffer
    (insert-file-contents file)
    (coq-mode)
    (setq-local coq-indent-box-style (string-match-p "boxed" file))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+" nil t) (replace-match ""))
    (let ((inhibit-message t))
      (if cache
          (indent-region (point-min) (point-max))
        (let ((indent-region-function nil))
          (indent-region (point-min) (point-max)))))
    (buffer-string)))

(ert-deftest coq-indent-region-cache-same ()
  "The token cache of `indent-region' does not change the indentation."
  (let ((files (directory-files indent-region-test-directory t
                                "\\`indent-.*\\.v\\'")))
    (should files)
    (dolist (file files)
      (should (equal (cons file (indent-region-test file t))
                     (cons file (indent-region-test file nil)))))
    ;; the cache lives during indent-region only
    (should-not coq-smie--tokens)))

(defun indent-region-test-match (branches indentation)
  "A match of BRANCHES branches, each indented by INDENTATION, a string."
  (concat "Definition f (n : nat) : nat :=\n"
          indentation "match n with\n"
          (mapconcat (lambda (i) (format "%s| %d => %d\n" indentation i i))
                     (number-sequence 0 (1- branches)) "")
          indentation "| _ => 0\n"
          indentation "end.\n"))

(ert-deftest coq-indent-region-cache-linear ()
  "With the token cache, `indent-region' computes each token about once.
Without it, each branch of a match reads again the tokens of all the
previous ones: about 17 computations per token for 30 branches, and
more for more branches."
  (let ((computed 0)
        (tokens 0))
    (cl-flet ((count (&rest _) (setq computed (1+ computed))))
      (with-temp-buffer
        (coq-mode)
        (insert (indent-region-test-match 30 ""))
        ;; the number of tokens of the buffer
        (goto-char (point-max))
        (while (let ((pos (point)))
                 (coq-smie-backward-token)
                 (< (point) pos))
          (setq tokens (1+ tokens)))
        (advice-add 'coq-smie--backward-token :before #'count)
        (unwind-protect
            (let ((inhibit-message t))
              (indent-region (point-min) (point-max)))
          (advice-remove 'coq-smie--backward-token #'count))
        (should (equal (buffer-string) (indent-region-test-match 30 "  ")))))
    (should (> tokens 100))
    (should (< computed (* 3 tokens)))))

;;; coq-test-indent-region.el ends here
