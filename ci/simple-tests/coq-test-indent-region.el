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

;;; coq-test-indent-region.el ends here
