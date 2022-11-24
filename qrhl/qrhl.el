;;; qrhl.el --- Mode for qrhl-tool theorem prover -*- lexical-binding: t -*-

;; This file is part of Proof General.

;; Copyright © 2017–2022  University of Tartu

;; Author: Dominique Unruh

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; See also https://dominique-unruh.github.io/qrhl-tool/

;;; Code:

(require 'proof)
(require 'proof-easy-config)
(require 'proof-script)                 ;For proof-generic-count-undos.
(require 'qrhl-input)


(defcustom qrhl-input-method "qrhl"
  "Input method to use when editing qRHL proof scripts"
  :type '(string) :group 'qrhl)

(defcustom qrhl-prog-name "qrhl"
  "Name/path of the qrhl-prover command. (Restart Emacs after changing this.)"
  :type '(string) :group 'qrhl)

(defcustom qrhl-indentation-level 2
  "Indentation level in qRHL scripts"
  :group 'qrhl)

(defun qrhl-find-and-forget (span)
  (proof-generic-count-undos span))
  
(defvar qrhl-focus-cmd-regexp
      (let* ((number "[0-9]+")
	     (white "[[:blank:]]*")
	     (number-or-range (concat number "\\(" white "-" white number "\\)?"))
	     (range-list (concat number-or-range "\\(" white "," white number-or-range "\\)*"))
	     (focus-label "\\({\\|}\\|[+*-]+\\)")
	     (focus-cmd (concat "\\(" range-list white ":" white "\\)?" focus-label))
	     )
	focus-cmd))

(defun qrhl-forward-regex (regex)
  "If text starting at point matches REGEX, move to end of the match and return t. 
Otherwise return nil"
  (and (looking-at regex) (goto-char (match-end 0)) t))

(defun qrhl-parse-regular-command ()
  "Find the period-terminated command starting at point.
Moves to its end.
Returns t if this worked."
  (let ((pos
	 (save-excursion
	   (progn
	    (while (or
	              ; skip forward over regular chars, period with non-white, quoted string
		    (qrhl-forward-regex "\\([^.{(\"]+\\|\\.[^ \t\n]\\|\"\\([^\"]+\\)\"\\)")
		    (and (looking-at "[{(]") (forward-list))
		    ))
	    (and (qrhl-forward-regex "\\.") (point))
	    ))))
    (and pos (goto-char pos) t)))

(defun qrhl-parse-focus-command ()
  (and (looking-at qrhl-focus-cmd-regexp)
       (goto-char (match-end 0))))

(defun qrhl-proof-script-parse-function ()
  "Finds the command/comment starting at the point"
  (or (and (qrhl-forward-regex "#[^\n]*\n") 'comment)
      (and (qrhl-parse-focus-command) 'cmd)
      (and (qrhl-parse-regular-command) 'cmd)))

(defvar qrhl-font-lock-subsuperscript
  '(("\\(⇩\\)\\([^⇩⇧[:space:]]\\)" .
     ((2 '(face subscript display (raise -0.3)))
      (1 '(face nil display ""))))
    ("\\(⇧\\)\\([^⇩⇧[:space:]]\\)" .
     ((2 '(face superscript display (raise 0.3)))
      (1 '(face nil display "")))))
  "Font-lock configuration for displaying sub/superscripts that are prefixed by ⇩/⇧")

(defvar qrhl-font-lock-keywords
  ; Regexp explanation: match the keyword/tactic after another command, and also if there are {}+*- in between (focusing commands)
  (cl-flet ((mk-regexp (word) (concat "\\(?:^\\|\\.[ \t]\\)[ \t{}+*-]*\\b\\(" word "\\)\\b")))
    (append qrhl-font-lock-subsuperscript
	    (mapcar (lambda (keyword) `(,(mk-regexp keyword) . (1 'font-lock-keyword-face)))
		    '("isabelle_cmd" "debug:" "isabelle" "quantum\\s +var" "classical\\s +var" "ambient\\s +var"
		      "program" "adversary" "qrhl" "lemma" "include" "qed" "cheat" "print\\s +goal" "print"))

	    (mapcar (lambda (tactic) `(,(mk-regexp tactic) . (1 'font-lock-function-name-face)))
		    '("admit" "wp" "sp" "swap" "simp" "rule" "clear" "skip" "inline" "seq" "conseq\\s +pre"
		      "conseq\\s +post" "conseq\\s +qrhl" "equal" "rnd" "rewrite"
		      "byqrhl" "casesplit" "case" "fix" "squash" "frame" "measure" "o2h" "semiclassical"
		      "sym" "local\\s +remove" "local\\s +up" "rename" "if" "isa"
		      ))

	    ; Regexp explanation: Match comment after
	    '(("\\(?:^\\|[ \t]\\)[ \t]*\\(#.*\\)" . (1 'font-lock-comment-face)))
	    ))
  "Font-lock configuration for qRHL proof scripts")

(defun qrhl-proof-script-preprocess (file start end cmd)
  "Strips comments from the command CMD.
Called before sending CMD to the prover."
  (list (replace-regexp-in-string "\\(?:^\\|[ \t]\\)[ \t]*#.*$" "" cmd)))

(proof-easy-config 'qrhl "qRHL"
  proof-prog-name qrhl-prog-name
  ;; We need to give some option here, otherwise `proof-prog-name' is
  ;; interpreted as a shell command which leads to problems if the file name
  ;; contains spaces (see the documentation for `proof-prog-name').
  qrhl-prog-args '("--emacs")
  ;;proof-script-command-end-regexp "\\.[ \t]*$"
  proof-script-parse-function #'qrhl-proof-script-parse-function
  proof-shell-annotated-prompt-regexp "^\\(\\.\\.\\.\\|qrhl\\)> "
  ;;proof-script-comment-start-regexp "#"
  ;;proof-script-comment-end "\n"
  proof-shell-error-regexp "^\\(\\[ERROR\\]\\|Exception\\)"
  proof-undo-n-times-cmd "undo %s."
  proof-find-and-forget-fn 'qrhl-find-and-forget
  proof-shell-start-goals-regexp "^[0-9]+ subgoals:\\|^Goal:\\|^No current goal\\.\\|^In cheat mode\\.\\|^No focused goals (use "
  proof-shell-proof-completed-regexp "^No current goal.$"
  proof-shell-eager-annotation-start "\\*\\*\\* "
  proof-shell-eager-annotation-start-length 4
  proof-no-fully-processed-buffer t
  proof-shell-filename-escapes '(("\\\\" . "\\\\") ("\"" . "\\\""))
  proof-shell-cd-cmd "changeDirectory \"%s\"."
  ;; ProofGeneral produces warning when this is not set.  But we don't want
  ;; goal/save commands to be recognized because that makes ProofGeneral do an
  ;; atomic undo.
  proof-save-command-regexp "\\`a\\`"   ;AKA `regexp-unmatchable' in Emacs-27
  proof-tree-external-display nil
  proof-script-font-lock-keywords qrhl-font-lock-keywords
  proof-goals-font-lock-keywords qrhl-font-lock-subsuperscript
  proof-response-font-lock-keywords qrhl-font-lock-keywords
  proof-shell-unicode t
  proof-script-preprocess #'qrhl-proof-script-preprocess
  )

; buttoning functions follow https://superuser.com/a/331896/748969
(define-button-type 'qrhl-find-file-button
  'follow-link t
  'action #'qrhl-find-file-button)

(defun qrhl-find-file-button (button)
  (find-file (buffer-substring (button-start button) (button-end button))))

(defun qrhl-buttonize-buffer ()
 "Turn all include commands in a qRHL proof script into clickable buttons"
 (interactive)
 (remove-overlays)
 (save-excursion
  (goto-char (point-min))
  (while (re-search-forward "include\s*\"\\([^\"]+\\)\"\s*\\." nil t)
   (make-button (match-beginning 1) (match-end 1) :type 'qrhl-find-file-button))))



(defun qrhl-current-line-rel-indent ()
  "Determins by how much to indent the current line relative to the previous
   indentation level. (Taking into account only the current line.)"
  (save-excursion
    (let ((qrhl-qed-pattern "^[ \t]*qed\\b")
	  (closing-brace-pattern "^[ \t]*}"))
      (beginning-of-line)
      ;; Analyse the current line and decide relative indentation accordingly
      (cond
       ;; qed - unindent by 2
       ((looking-at qrhl-qed-pattern) (- qrhl-indentation-level))
       ;; } - unindent by 2
       ((looking-at closing-brace-pattern) (- qrhl-indentation-level))
       ;; indent as previous
       (t 0)))))

(defun qrhl-indent-line ()
  "Indent current line as qRHL proof script"
  (interactive)
  
  (let ((not-found t) (previous-indent nil) (previous-offset 0) rel-indent
	(lemma-pattern "^[ \t]*\\(lemma\\|qrhl\\)\\b")
	(comment-pattern "^[ \t]*#")
	(empty-line-pattern "^[ \t]*$")
	(brace-pattern "^[ \t]*{")
	(focus-pattern "^[ \t{}+*-]*[+*-][ \t]*"))

    (beginning-of-line) ;; Throughout this function, we will always be at the beginning of a line

    ;; Identify preceding indented line (relative to which we indent)
    (save-excursion
      (while (and not-found (not (bobp)))
	(forward-line -1)
	(cond
	 ((and (not previous-indent) (looking-at comment-pattern))
	  (setq previous-indent (current-indentation)))
	 ((looking-at empty-line-pattern) ())
	 (t
	  (progn
	   (setq previous-indent (current-indentation))
	   (setq not-found nil)
	   (cond
	    ((looking-at lemma-pattern) (setq previous-offset qrhl-indentation-level))
	    ((looking-at focus-pattern) (setq previous-offset (- (match-end 0) (point) previous-indent)))
	    ((looking-at brace-pattern) (setq previous-offset qrhl-indentation-level)))
	  )))))
    (if (not previous-indent) (setq previous-indent 0))
    
    ;; Now previous-indent contains the indentation-level of the preceding non-comment non-blank line
    ;; If there is such line, it contains the indentation-level of the preceding non-blank line
    ;; If there is no such line, it contains 0

    ;; And previous-offset contains the offset that that line adds to following lines
    ;; (i.e., 0 for normal lines, positive for qed and {, negative for })

    (setq rel-indent (qrhl-current-line-rel-indent))
    
    ;; Indent relative to previous-indent by rel-indent and previous-offset
    (indent-line-to (max (+ previous-indent rel-indent previous-offset) 0))))


(add-hook 'qrhl-mode-hook
	  (lambda ()
	    (set-input-method qrhl-input-method)
	    (setq electric-indent-inhibit t)  ;; Indentation is not reliable enough for electric indent
	    (setq indent-line-function 'qrhl-indent-line)
	    ;; This ensures that the fontification from qrhl-font-lock-subsuperscript is updated correctly
	    ;; when editing text (when re-fontifying).
	    ;; We only add it in qrhl-mode, not qrhl-response-mode or qrhl-goals-mode because in the latter two,
	    ;; text is never edited, only replaced as a while, so refontification doesn't happen there and
	    ;; is not needed.
	    (setq-local font-lock-extra-managed-props '(display))
	    (qrhl-buttonize-buffer)))

(provide 'qrhl)
