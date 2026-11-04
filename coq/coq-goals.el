;;; coq-goals.el --- Folding of the goals of Coq  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; In the goals buffer, the goals after the first one are folded to a
;; line giving their number, with the number of unfocused goals when
;; Coq displays them (Set Printing Unfocused).  Clicking the line (or
;; RET on it) unfolds them.  The mode line of the goals buffer gives
;; the number of goals, see `coq-goals-mode-line'.  When the goals do
;; not fit in their window, the last hypotheses and the conclusion of
;; the first goal are displayed, see `coq-goals-show-first-goal'.
;;
;; This file works on the text of the goals buffer, as Coq prints it:
;; it depends on no proof shell.

;;; Code:

(require 'subr-x)
(require 'seq)

(defcustom coq-prefer-top-of-conclusion nil
  "Prefer start of the conclusion over its end when displaying large goals.
Namely, goals that do not fit in the goals window."
  :type 'boolean
  :group 'coq)

(defcustom coq-fold-other-goals t
  "*If non-nil, the goals other than the first one are folded.
Only their number is displayed, with the number of unfocused goals if
Coq displays them (see Set Printing Unfocused), on a line that unfolds
them when clicked, see `coq-toggle-other-goals'."
  :type 'boolean
  :group 'coq)

(defcustom coq-auto-adapt-printing-width t
  "If non-nil, adapt the printing width of Coq to the goals window.
With Proof General, each time the user sends a bunch of commands to
Coq, check if the width of the goals window changed, and set Coq
printing width (Set Printing Width).  If several windows display the
goals buffer, one is chosen arbitrarily.  When backtracking, Coq
restores the printing width of the state it goes back to: the width is
adapted again at the next command.

With Eglot, the goals are printed at the width of their window, and
printed again when it changes.  The answers of the queries are printed
at the width of the response window."
  :type 'boolean
  :safe 'booleanp
  :group 'coq)

(defcustom coq-optimise-resp-windows-enable t
  "If non-nil (default), resize the goals and response windows to their content.
After each command, the response window gets the height of its content,
the goals window the rest, when they are one above the other.

With Proof General, only in three windows mode.  With Eglot, when the
conclusion is apart (see `coq-eglot-split-goals'), the hypotheses get
the height they need too, the conclusion window gets the rest."
  :type 'boolean
  :group 'coq)

(defface coq-other-goals-face '((t :inherit shadow))
  "Face of the line giving the number of the other goals.
See `coq-fold-other-goals'."
  :group 'proof-faces)

(defface coq-goals-count-face '((t :inherit mode-line-emphasis))
  "Face of the number of goals in the mode line of the goals buffer.
See `coq-goals-mode-line'."
  :group 'proof-faces)

(defconst coq-other-goal-regexp "^\\(?:sub\\)?goal [0-9]+ "
  "Regexp matching the beginning of a goal after the first one.")

(defconst coq-goals-separator-regexp "^[ \t]*=\\{4,\\}[ \t]*$"
  "Regexp matching the line between the hypotheses and the conclusion.")

(defconst coq-unfocused-goals-regexp "^\\*\\*\\* Unfocused goals:"
  "Regexp matching the line before the unfocused goals.")

(defconst coq-subproof-complete-regexp "This subproof is complete"
  "Regexp matching the beginning of the goals when no goal is focused.
The goals displayed after are all unfocused.")

(defun coq-goals--subproof-complete-p ()
  "Non-nil if no goal is focused: the goals displayed are unfocused."
  (save-excursion
    (goto-char (point-min))
    (looking-at coq-subproof-complete-regexp)))

(defvar coq-other-goals-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'coq-toggle-other-goals)
    (define-key map (kbd "RET") #'coq-toggle-other-goals)
    map)
  "Keymap of the line giving the number of the other goals.")

(defun coq-goals--count (regexp start end)
  "Number of lines matching REGEXP between START and END."
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (re-search-forward regexp end t)
        (setq count (1+ count)))
      count)))

(defun coq-goals--other-goals-start ()
  "Start of the goals after the first one in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "\\(?:" coq-other-goal-regexp "\\)\\|\\(?:"
                                     coq-unfocused-goals-regexp "\\)")
                             nil t)
      (match-beginning 0))))

(defun coq-goals--summary (start)
  "Line giving the number of the goals from START, folded or not."
  (let* ((unfocused-start (if (coq-goals--subproof-complete-p)
                              start
                            (save-excursion
                              (goto-char start)
                              (and (re-search-forward coq-unfocused-goals-regexp
                                                      nil t)
                                   (match-beginning 0)))))
         (others (coq-goals--count coq-other-goal-regexp start
                                   (or unfocused-start (point-max))))
         (unfocused (if unfocused-start
                        (coq-goals--count coq-other-goal-regexp
                                          unfocused-start (point-max))
                      0))
         (plural (lambda (n) (if (= n 1) "" "s"))))
    (propertize
     (concat (if coq-fold-other-goals "▸ " "▾ ")
             (string-join
              (delq nil
                    (list (when (> others 0)
                            (format "%d more goal%s" others
                                    (funcall plural others)))
                          (when (> unfocused 0)
                            (format (if (> others 0) "%d unfocused"
                                      "%d unfocused goal%s")
                                    unfocused (funcall plural unfocused)))))
              ", "))
     'face 'coq-other-goals-face
     'mouse-face 'highlight
     'help-echo "mouse-1, RET: show or hide the other goals"
     'keymap coq-other-goals-map)))

(defun coq-goals-fold-other-goals ()
  "Fold the goals after the first one in the current goals buffer.
Or unfold them, see `coq-fold-other-goals'.  In both cases, a line
gives their number."
  (remove-overlays (point-min) (point-max) 'coq-other-goals t)
  (let ((start (coq-goals--other-goals-start)))
    (when start
      (let ((ov (make-overlay start (point-max) nil nil t)))
        (overlay-put ov 'coq-other-goals t)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'keymap coq-other-goals-map)
        ;; folded: the line replaces the goals
        (overlay-put ov (if coq-fold-other-goals 'display 'before-string)
                     (concat (coq-goals--summary start)
                             (if coq-fold-other-goals "" "\n")))))))

(defun coq-goals-folded-start ()
  "Start of the folded goals in the current goals buffer, or nil."
  (let ((ov (seq-find (lambda (ov) (overlay-get ov 'coq-other-goals))
                      (overlays-in (point-min) (point-max)))))
    (and ov (overlay-get ov 'display) (overlay-start ov))))

(defvar coq-goals-buffer-function nil
  "Function returning the goals buffer of the current backend.")

(defvar-local coq-ask-printing-width-function nil
  "Command asking for a printing width and printing the goals at it.
Set by each backend, called interactively by `coq-ask-printing-width'.")

(defun coq-ask-printing-width ()
  "Ask for a printing width and print the goals at this width.
The backend does it, see `coq-ask-printing-width-function'.  See also
`coq-auto-adapt-printing-width'."
  (interactive)
  (unless coq-ask-printing-width-function
    (user-error "Coq: no backend to set the printing width in this buffer"))
  (call-interactively coq-ask-printing-width-function))

(defun coq-toggle-other-goals ()
  "Fold or unfold the goals other than the first one.
See `coq-fold-other-goals'."
  (interactive)
  (setq coq-fold-other-goals (not coq-fold-other-goals))
  (let ((buffer (and coq-goals-buffer-function
                     (funcall coq-goals-buffer-function))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (coq-goals-fold-other-goals))))
  (message "Other goals %s" (if coq-fold-other-goals "folded" "unfolded")))

(defun coq-goals-count ()
  "Numbers of goals of the current goals buffer, as (FOCUSED . UNFOCUSED).
UNFOCUSED is a string if Coq gives it per level.  Set Printing
Unfocused is supposed to be on: if no unfocused goal is displayed,
there is none.  Return nil outside of proofs."
  (save-excursion
    (goto-char (point-min))
    (let ((focused
           (cond
            ((looking-at "\\([0-9]+\\) \\(?:focused \\)?\\(?:sub\\)?goals?")
             (string-to-number (match-string 1)))
            ((looking-at (concat "No more \\(?:sub\\)?goals\\|"
                                 coq-subproof-complete-regexp))
             0))))
      (when focused
        (cons focused
              (cond
               ((re-search-forward "(unfocused: \\([^)]*\\))" nil t)
                (match-string-no-properties 1))
               ;; with Set Printing Unfocused, the number of unfocused
               ;; goals follows
               ((and (coq-goals--subproof-complete-p)
                     (re-search-forward
                      "^\\([0-9]+\\) \\(?:sub\\)?goals?$" nil t))
                (string-to-number (match-string 1)))
               ((re-search-forward coq-unfocused-goals-regexp nil t)
                (coq-goals--count coq-other-goal-regexp (point) (point-max)))
               ;; Set Printing Unfocused is supposed to be on: none
               (t 0)))))))

(defun coq-goals-mode-line ()
  "Number of goals of the current goals buffer, for its mode line.
Short: \"3+4\" for 3 focused and 4 unfocused goals, in
`coq-goals-count-face'.  The help echo says it in words."
  (let ((count (coq-goals-count)))
    (if (not count)
        ""
      (concat
       " "
       (propertize (format "%d+%s" (car count) (cdr count))
                   'face 'coq-goals-count-face
                   'help-echo (format "Goals: %d focused, %s unfocused"
                                      (car count) (cdr count)))))))

(defun coq-goals--separator ()
  "Start of the line of the separator of the first goal, or nil.
Nil also if no goal is focused: the goals displayed are unfocused."
  (unless (coq-goals--subproof-complete-p)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward coq-goals-separator-regexp nil t)
        (match-beginning 0)))))

(defun coq-goals-window-start (window)
  "Start of WINDOW showing the first goal of its goals buffer.
The end of its conclusion is at the bottom of WINDOW, maximizing the
number of hypotheses displayed: the line of the folded goals after it
is not worth a line of the first goal.  If the conclusion does not fit
and `coq-prefer-top-of-conclusion' is non-nil, the separator is on the
first line instead.  The beginning of the buffer if no goal is
focused."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (let ((separator (coq-goals--separator)))
        (if (not separator)
            (point-min)
          (goto-char (or (coq-goals--other-goals-start) (point-max)))
          (skip-chars-backward " \t\n")
          (vertical-motion (- 1 (window-body-height window)) window)
          (if (and coq-prefer-top-of-conclusion (> (point) separator))
              separator
            (point)))))))

(defun coq-goals-show-first-goal (buffer)
  "Scroll the windows of goals BUFFER to show the first goal.
See `coq-goals-window-start'.  Point does not move, unless it would be
out of view."
  (dolist (window (get-buffer-window-list buffer nil t))
    (set-window-start window (coq-goals-window-start window))))

(provide 'coq-goals)

;;; coq-goals.el ends here
