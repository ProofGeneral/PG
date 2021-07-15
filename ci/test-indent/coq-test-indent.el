;;; Regression testing of indentation.

;;; Initially the file is indented as it is supposed to be. Each line
;;; is unindented, then indented. If the new indentation differ a
;;; comment is added to signal it.

;;; WARNING: there are relative path names, so this currently works
;;; only frowhen the current directory is the one containin this file.

(defun blank-line-p ()
  (= (current-indentation)
     (- (line-end-position) (line-beginning-position))))


(defun line-fixindent ()
  (save-excursion 
    (let ((end (line-end-position)))
      (re-search-forward "fixindent" end t))))


(defun test-indent-line (&optional nocomment)
  (interactive)
  (unless (or (blank-line-p) (line-fixindent))
    (back-to-indentation)
    (let ((init-col (current-column)))
      ;; avoid moving comments 
      (unless (proof-inside-comment (point)) (delete-horizontal-space))
      (indent-according-to-mode)
      (let ((res (- (current-column) init-col)))
        (unless (= 0 res)
          (save-excursion
            (end-of-line)
            (unless nocomment
              (insert (format " (*<=== %s%d INDENT CHANGED *)"
                              (if (< res 0) "" "+")
                              res))))
          res)
        0))))


(defun remove-previous-comment ()
  (interactive)
  (save-excursion
    (end-of-line)
    (let* ((com (re-search-backward " (\\*<===" (line-beginning-position) t))
           (strt (and com (point)))
           (end (and com (line-end-position))))
      (when com (delete-region strt end)))))


(defun test--indent-region (beg end boxed &optional nocomment)
  (let ((line-move-visual nil)
        ;; we store the last line number rather than the position
        ;; since by inderting things we shift the end pos, but not the
        ;; end line.
        (last-line (line-number-at-pos end))
        (stop nil))
    (set (make-local-variable 'coq-indent-box-style) boxed)
    (goto-char beg)
    (while (and (<= (line-number-at-pos) last-line)
                (not stop))
      (remove-previous-comment)
      (test-indent-line nocomment)
      (setq stop (/= (forward-line) 0)))))

(defun remove-comments-region (beg end)
  (interactive "r")
  (goto-char beg)
  (let ((line-move-visual nil))
    (while (< (point) end);; loops because end position changes.
      (remove-previous-comment)
      (forward-line))))


(defun test-indent-region (beg end &optional boxed nocomment)
  (interactive "r\nP")
  (test--indent-region beg end boxed nocomment))


(defun test-indent-region-boxed (beg end &optional nocomment)
  (interactive "r")
  (test--indent-region beg end t nocomment))


(defun test-indent-buffer (&optional nocomment) (interactive)
       (test--indent-region (point-min) (point-max) nil nocomment))

(defun test-indent-buffer-boxed (&optional nocomment) (interactive)
       (test--indent-region (point-min) (point-max) t nocomment))

(defun launch-test (orig indented &optional boxed)
    (load-file "../../generic/proof-site.el")
    (find-file orig)
    (write-file indented)
    (if boxed (test-indent-buffer-boxed t) (test-indent-buffer t))
    (write-file indented))
