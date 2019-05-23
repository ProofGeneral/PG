;;; coq-diffs.el --- highlight text marked with XML-like tags for Coq diffs

;; This file is part of Proof General.

;; Portions © Copyright 2019  Jim Fehrle

;; Author:      Jim Fehrle <jim.fehrle@gmail.com>

;; License:  BSD-3 (3-Clause BSD License)

;;; Commentary:
;; 

(require 'coq-db)

;;; Code:

(defun coq-insert-with-face (str face)
  (let ((start (point)))
    (insert str)
    (if face
      (overlay-put (span-make start (point-max)) 'face face))))

(defun coq-insert-tagged-text (str)
"Insert text into the current buffer applying faces specified by tags.

For example '<diff.added>foo</diff.added>' inserts 'foo' in the buffer
and applies the appropriate face.

coq-tag-map defines the mapping from tag name to face."
  (let* ((len (length str))
         (off 0)
	     (fstack)
	     (rhs))
    (while (< off len)
      (string-match "^\\([ \t]*\\)\\(.*\n?\\)" str off)
      (setq off (match-end 0))
      (coq-insert-with-face (match-string 1 str) nil)  ;; begin-line white space
      (setq rhs (match-string 2 str))
      (string-match "[ \t\n]*$" rhs)
      (let* ((end-white (match-string 0 rhs))  ;; end-line white space
             (line (substring rhs 0 (- (length rhs) (length end-white))))
             (llen (length line))
             (loff 0))
        (while (< loff llen)
          (if (> loff 0)
            (aset line (1- loff) ?\n))   ;; only way to get an anchored search midstring
          (cond
            ; make sure that a) the matched string is never the empty string, and
            ; b) that every non-empty string has a match
            ((string-match "^<\\(/?\\)\\([a-zA-Z\\.]+\\)>" line loff)    ;; tag
              (let* ((end-mark (match-string 1 line))
                     (tag (match-string 2 line))
                     (face (cdr (assoc tag coq-tag-map))))
                (if face
                  (setq fstack (if (equal end-mark "") (cons face fstack) (cdr fstack)))
                  (coq-insert-with-face (match-string 0 line) (car fstack)))))    ;; unknown tag, show as-is
            ((string-match "^<?[^<\n]+" line loff)    ;; text
              (coq-insert-with-face (match-string 0 line) (car fstack))))
          (setq loff (match-end 0)))
        (coq-insert-with-face end-white nil)))))    ; end-line white space

(provide 'coq-diffs)

;;; coq-diffs.el ends here
