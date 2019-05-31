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
        ;; FIXME: Why not (put-text-property start (point) 'face face)?
        (overlay-put (span-make start (point)) 'face face))))

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

  (let* ((len (length str))
         (off 0)
	 (fstack)
	 (rhs))
    ;; FIXME: It would likely be better to insert `str' into the buffer
    ;; and then work in the buffer instead of manipulating strings.
    (while (< off len)
      (string-match "^\\([ \t]*\\)\\(.*\n?\\)" str off)
      (setq off (match-end 0))
      (insert (match-string 1 str)) ;; begin-line white space
      (setq rhs (match-string 2 str))
      (string-match "[ \t\n]*\\'" rhs)
      (let* ((end-white (match-string 0 rhs)) ;; end-line white space
             (line (substring rhs 0 (- (length rhs) (length end-white))))
             (llen (length line))
             (loff 0))
        (while (and (< loff llen)
                    (string-match "<\\(/\\)?\\([a-zA-Z\\.]+\\)>" line loff))
          (let* ((end-mark (match-beginning 1))
                 (tag (match-string 2 line))
                 (face (cdr (assoc tag coq-tag-map)))
                 (start (match-beginning 0))
                 (end (match-end 0)))
            (coq-insert-with-face (substring line loff start) (car fstack))
            (setq loff end)
            (if face
                (setq fstack (if end-mark (cdr fstack) (cons face fstack)))
              ;; Unknown tag, show as-is!
              (coq-insert-with-face (substring line start end) (car fstack)))))
        (coq-insert-with-face (substring line loff) (car fstack))
        (insert end-white)))))    ; end-line white space

(provide 'coq-diffs)

;;; coq-diffs.el ends here
