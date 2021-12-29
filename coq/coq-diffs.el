;;; coq-diffs.el --- highlight text marked with XML-like tags for Coq diffs  -*- lexical-binding: t; -*-

;; This file is part of Proof General.

;; Portions © Copyright 2019  Jim Fehrle

;; Author:      Jim Fehrle <jim.fehrle@gmail.com>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; 

(require 'coq-db)

;;; Code:

(defun coq-highlight-with-face (end face)
  (if face
    ;; (put-text-property (point) end 'face face) doesn't work
    (overlay-put (span-make (point) end) 'face face))
  (goto-char end))

(defun coq-search (regex limit)
  (save-excursion
     (re-search-forward regex limit t)))

(defun coq-insert-tagged-text (str)
  "Insert text into the current buffer applying faces specified by tags.

For example '<diff.added>foo</diff.added>' inserts 'foo' in the buffer
and applies the appropriate face.

`coq-tag-map' defines the mapping from tag name to face."
;; Coq inserts tags before splitting into lines.  Avoid highlighting
;; white space at the beginning or end of lines in a conclusion or
;; hypothesis that's split across multiple lines.

;; Doesn't handle the unlikely case of escaping regular text
;; that looks like a tag.  Unknown tags such as "<foo>" are
;; shown as-is.  The user can turn off diffs in this very unlikely case.

  (let* ((fstack)
      (start (point))
      (strend)
      (lend)
      (end-white-begin))
    (insert str)
    (setq strend (copy-marker (point)))
    (goto-char start)
    (while (< (point) strend)
      (coq-search "^\\([ \t]*\\).*\\(\n\\)?" strend)
      (setq lend (copy-marker (match-end 0)))
      (if (match-end 1)
        (goto-char (match-end 1)))  ;; begin-line white space
      (let* ((nl (if (match-string 2) 1 0))
              (end-white-len    ;; length of end-line white space
                (if (coq-search "[ \t\n]*\\'" lend)
                  (- (match-end 0) (match-beginning 0))
                  0)))
        (setq end-white-begin (copy-marker (- (- lend end-white-len) nl)))

        (while (and (< (point) lend)
                    (coq-search "<\\(/\\)?\\([a-zA-Z\\.]+\\)>" lend))
          (let* ((close-tag (match-beginning 1))
               (tag (match-string 2))
               (face (cdr (assoc tag coq-tag-map)))
               (start (match-beginning 0))
               (end (match-end 0)))
            (coq-highlight-with-face start (car fstack))  ;; text before tag
            (if face
              (progn
                (replace-match "")
                (setq fstack (if close-tag (cdr fstack) (cons face fstack))))
              ;; Unknown tag, show as-is!
              (coq-highlight-with-face end (car fstack)))))
        (coq-highlight-with-face end-white-begin (car fstack))  ;; text after last tag
        (goto-char lend)))))    ;; end-line white space

(provide 'coq-diffs)

;;; coq-diffs.el ends here
