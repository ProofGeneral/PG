;;; coq-goals.el --- Display of the goals of Coq  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 2001-2017, 2026  Pierre Courtieu
;; Portions © Copyright 2019  Jim Fehrle (formerly coq-diffs.el)

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
(require 'pg-pamacs)                    ; proof-deftoggle
(require 'coq-db)                       ; faces of the buttons
(require 'span)                         ; span-make, for the diffs
(require 'coq-syntax)                   ; coq-detect-hyps-positions

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

(proof-deftoggle coq-auto-adapt-printing-width)

(defcustom coq-show-proof-stepwise nil
  "Display the proof terms stepwise in the *response* buffer.
This option can be combined with option `coq-diffs'.
It is mostly useful in three window mode, see also
`proof-three-window-mode-policy' for details."

  :type 'boolean
  :safe 'booleanp
  :group 'coq-auto-compile)

(proof-deftoggle coq-show-proof-stepwise)

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

(defvar coq-response-buffer-function nil
  "Function returning the response buffer of the current backend.")

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

;;; Frozen copies of the goals and response buffers

(defconst coq-store-buffer-name "*response-freeze*"
  "Name of the buffer where `coq-store-goals-win' copies the goals.
And `coq-store-response-win' the response.")

(defun coq-store-buffer-win (buffer &optional erase)
  "Copy BUFFER at the top of the buffer `coq-store-buffer-name'.
Erase it first if ERASE is non-nil.  Display it in another frame,
unless a visible window displays it already.  It is in the mode of the
response buffer, see `coq-response-buffer-function'."
  (when (buffer-live-p buffer)
    (let* ((existing (get-buffer coq-store-buffer-name))
           (frozen (get-buffer-create coq-store-buffer-name))
           (response (and coq-response-buffer-function
                          (funcall coq-response-buffer-function))))
      (with-current-buffer frozen
        (unless existing
          (when (buffer-live-p response)
            (funcall (buffer-local-value 'major-mode response)))
          (read-only-mode 0))
        (goto-char (point-min))
        (insert "\n************************************\n")
        (goto-char (point-min)))
      (with-current-buffer buffer
        (if erase
            (copy-to-buffer frozen (point-min) (point-max))
          (append-to-buffer frozen (point-min) (point-max))))
      (set-window-point (or (get-buffer-window frozen 'visible)
                            (display-buffer-other-frame frozen))
                        (with-current-buffer frozen (point-min))))))

(defun coq-store-response-win (&optional erase)
  "Copy the response buffer at the top of the buffer `coq-store-buffer-name'.
Erase it first with a prefix argument ERASE."
  (interactive "P")
  (coq-store-buffer-win (and coq-response-buffer-function
                             (funcall coq-response-buffer-function))
                        erase))

(defun coq-store-goals-win (&optional erase)
  "Copy the goals buffer at the top of the buffer `coq-store-buffer-name'.
Erase it first with a prefix argument ERASE."
  (interactive "P")
  (coq-store-buffer-win (and coq-goals-buffer-function
                             (funcall coq-goals-buffer-function))
                        erase))

(define-obsolete-function-alias 'proof-store-buffer-win
  #'coq-store-buffer-win "4.6")
(define-obsolete-function-alias 'proof-store-response-win
  #'coq-store-response-win "4.6")
(define-obsolete-function-alias 'proof-store-goals-win
  #'coq-store-goals-win "4.6")


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

(defvar-local coq-goals-count-function nil
  "Function returning the numbers of goals of the current goals buffer.
If non-nil, `coq-goals-count' calls it without argument instead of
reading the goals in the buffer, for backends that know them.  It
returns the same values.")

(defun coq-goals-count ()
  "Numbers of goals of the current goals buffer, as (FOCUSED . UNFOCUSED).
UNFOCUSED is a string if Coq gives it per level.  Set Printing
Unfocused is supposed to be on: if no unfocused goal is displayed,
there is none.  Return nil outside of proofs.  See also
`coq-goals-count-function'."
  (if coq-goals-count-function
      (funcall coq-goals-count-function)
    (coq-goals--read-count)))

(defun coq-goals--read-count ()
  "Numbers of goals of the current goals buffer, read in its text.
See `coq-goals-count'."
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


;;; Folding and highlighting of the hypotheses
;;
;; The hypotheses of the first goal can be folded (their type is
;; hidden) and highlighted.  Both stay in the next goals: folding
;; always, highlighting if `coq-highlight-hyps-sticky'.  A Search
;; highlights the hypotheses it finds.
;;
;; Common to all the Coq backends.  A backend marks
;; each hypothesis of its goals buffer with two text properties:
;; `coq-hyp', the list of its names, on the whole hypothesis from the
;; beginning of its line, and `coq-hyp-body', on the part after the
;; names (" : type" or " := definition : type").  Then it calls
;; `coq-hyps-decorate', which displays the folding buttons, the folded
;; and highlighted hypotheses.  The commands work on these properties
;; only.  A backend sets also `coq-goals-buffer-function' and
;; `coq-response-buffer-function'.
;; Formerly coq-hyps.el.

(declare-function coq-id-at-point "coq-queries")

(defvar coq-highlight-hyps-cited-in-response t
  "If non-nil, a Search highlights the hypotheses it finds.")

(defvar coq-highlight-hyps-sticky nil
  "If non-nil, the highlighted hypotheses stay so in the next goals.")

(defvar coq-highlighted-hyps nil
  "Names of the highlighted hypotheses.")

(defvar coq-highlighted-hyps-bg "gray"
  "Background color of the highlighted hypotheses.")

(defvar coq-hidden-hyps nil
  "Names of the folded hypotheses, whose type is hidden.
They stay folded in the next goals.")

(defvar coq-hypcross-hovering-help t
  "Whether the folding buttons of the hypotheses show a help when hovered.")

(defvar coq-hypname-map (make-sparse-keymap)
  "Keymap of the names of the hypotheses.")

(defvar coq-hypcross-map (make-sparse-keymap)
  "Keymap of the folding buttons of the hypotheses.")

(defvar coq-hidden-hyp-map (make-sparse-keymap)
  "Keymap of the folded hypotheses.")


;;; The hypotheses of the goals buffer

(defun coq-hyps--goals-buffer ()
  "The buffer holding the hypotheses, or nil."
  (let ((buffer (and coq-goals-buffer-function
                     (funcall coq-goals-buffer-function))))
    (and (buffer-live-p buffer) buffer)))

(defun coq-hyps-mark (names start body end)
  "Mark the text from START to END as hypothesis NAMES.
The part after the names starts at BODY.  For backends that find the
hypotheses in the text of the goals."
  (with-silent-modifications
    (put-text-property start end 'coq-hyp names)
    (put-text-property body end 'coq-hyp-body t)))

(defun coq-hyps-regions ()
  "List the hypotheses of the goals buffer, as (NAMES START BODY END).
START is the beginning of the line of the hypothesis, BODY the end of
its names."
  (let ((buffer (coq-hyps--goals-buffer))
        regions)
    (when buffer
      (with-current-buffer buffer
        (let ((pos (point-min)))
          (while (setq pos (text-property-not-all pos (point-max) 'coq-hyp nil))
            (let ((end (next-single-property-change pos 'coq-hyp nil (point-max))))
              (push (list (get-text-property pos 'coq-hyp) pos
                          (or (text-property-any pos end 'coq-hyp-body t) end)
                          end)
                    regions)
              (setq pos end))))))
    (nreverse regions)))

(defun coq-hyps-names ()
  "Names of the hypotheses of the goals buffer."
  (mapcan (lambda (region) (copy-sequence (car region))) (coq-hyps-regions)))


;;; Display

(defun coq-hyps--cross (folded)
  "The text of a folding button, for a FOLDED hypothesis if non-nil."
  ;; not a button: it would be wider than a character
  (if folded
      (propertize "+" 'face 'coq-button-face-pressed
                  'mouse-face 'coq-button-face-active)
    (propertize "-" 'face 'coq-button-face
                'mouse-face 'coq-button-face-active)))

(defun coq-hyps--overlay (start end &rest properties)
  "Make an overlay from START to END with PROPERTIES."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'coq-hyp t)
    (overlay-put ov 'evaporate t)
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    ov))

(defun coq-hyps--type-start (body end)
  "Start of the type of a hypothesis, after the separator at BODY.
After \":=\" for a definition.  END is the end of the hypothesis."
  (save-excursion
    (goto-char body)
    (skip-chars-forward " \n:=" end)
    (point)))

(defun coq-hyps-decorate (&optional new)
  "Display the folding buttons, the folded and highlighted hypotheses.
NEW non-nil means the goals buffer shows new goals: the highlighting
stops there unless `coq-highlight-hyps-sticky'."
  (when (and new (not coq-highlight-hyps-sticky))
    (setq coq-highlighted-hyps nil))
  (let ((buffer (coq-hyps--goals-buffer)))
    (when buffer
      (with-current-buffer buffer
        (remove-overlays (point-min) (point-max) 'coq-hyp t)
        (pcase-dolist (`(,names ,start ,body ,end) (coq-hyps-regions))
          (let ((folded (seq-intersection names coq-hidden-hyps))
                (names-start (save-excursion
                               (goto-char start)
                               (skip-chars-forward " ")
                               (point)))
                (type-start (coq-hyps--type-start body end)))
            (when (< start names-start)
              (coq-hyps--overlay start (1+ start)
                                 'display (coq-hyps--cross folded)
                                 'keymap coq-hypcross-map
                                 'help-echo
                                 (and coq-hypcross-hovering-help
                                      "mouse-1: (un)fold; mouse-2: insert the name")))
            (coq-hyps--overlay names-start body 'keymap coq-hypname-map)
            (when (seq-intersection names coq-highlighted-hyps)
              (coq-hyps--overlay type-start end
                                 'face `(:background ,coq-highlighted-hyps-bg)))
            (when folded
              (coq-hyps--overlay type-start end
                                 'display (make-string
                                           (min 8 (- end type-start)) ?.)
                                 'keymap coq-hidden-hyp-map
                                 'mouse-face 'proof-command-mouse-highlight-face
                                 'help-echo
                                 "mouse-1: unfold; mouse-2: insert the name"))))))))


;;; The hypothesis at point

(defun coq-hyps-at (pos)
  "Names of the hypothesis at POS of the goals buffer, or nil."
  (or (get-text-property pos 'coq-hyp)
      (and (> pos (point-min)) (get-text-property (1- pos) 'coq-hyp))))

(defun coq-hyps--at-point ()
  "Names of the hypothesis at point, or nil.
In the goals buffer, the one under point.  Elsewhere, the identifier at
point, if it is a hypothesis of the goals buffer."
  (if (eq (current-buffer) (coq-hyps--goals-buffer))
      (coq-hyps-at (point))
    (let ((id (coq-id-at-point)))
      (when id
        (seq-some (lambda (region) (and (member id (car region)) (car region)))
                  (coq-hyps-regions))))))

(defun coq-hyps--at-mouse (event)
  "Names of the hypothesis at the position of mouse EVENT, or nil."
  (let ((pos (posn-point (event-start event))))
    (with-current-buffer (window-buffer (posn-window (event-start event)))
      (and pos (coq-hyps-at pos)))))

(defun coq-hyps--toggle (var names verb)
  "Toggle NAMES in the names of VAR, for VERB."
  (unless names (user-error "Coq: no hypothesis at point"))
  (set var (if (seq-intersection names (symbol-value var))
               (seq-difference (symbol-value var) names)
             (append names (symbol-value var))))
  (coq-hyps-decorate)
  (message "Coq: %s %s" (string-join names ", ")
           (if (seq-intersection names (symbol-value var)) verb
             (concat "un" verb))))

(defun coq-hyps--names-of (h)
  "Names of the hypothesis named H, or just H if not displayed."
  (or (seq-find (lambda (names) (member h names))
                (mapcar #'car (coq-hyps-regions)))
      (list h)))


;;; Folding

(defun coq-toggle-fold-hyp-at-point ()
  "Fold or unfold the hypothesis at point: hide its type.
In the goals buffer, the hypothesis under point, elsewhere the one
named by the identifier at point.  It stays folded in the next goals."
  (interactive)
  (coq-hyps--toggle 'coq-hidden-hyps (coq-hyps--at-point) "folded"))

(defun coq-toggle-fold-hyp-at-mouse (event)
  "Fold or unfold the hypothesis at mouse EVENT."
  (interactive "e")
  (coq-hyps--toggle 'coq-hidden-hyps (coq-hyps--at-mouse event) "folded"))

(defun coq-toggle-fold-hyp (h)
  "Fold or unfold the hypothesis named H (asked interactively)."
  (interactive "sHypothesis to fold: ")
  (coq-hyps--toggle 'coq-hidden-hyps (coq-hyps--names-of h) "folded"))

(defun coq-fold-hyp (h)
  "Fold the hypothesis named H: hide its type, in the next goals too."
  (interactive "sHypothesis to fold: ")
  (unless (member h coq-hidden-hyps)
    (coq-toggle-fold-hyp h)))

(defun coq-unfold-hyp (h)
  "Unfold the hypothesis named H."
  (interactive "sHypothesis to unfold: ")
  (when (member h coq-hidden-hyps)
    (coq-toggle-fold-hyp h)))

(defun coq-unfold-hyps ()
  "Unfold all the hypotheses."
  (interactive)
  (setq coq-hidden-hyps nil)
  (coq-hyps-decorate))


;;; Highlighting

(defun coq-toggle-highlight-hyp-at-point ()
  "Highlight or unhighlight the hypothesis at point.
See `coq-toggle-fold-hyp-at-point' for the hypothesis at point, and
`coq-highlight-hyps-sticky'."
  (interactive)
  (coq-hyps--toggle 'coq-highlighted-hyps (coq-hyps--at-point) "highlighted"))

(defun coq-toggle-highlight-hyp (h)
  "Highlight or unhighlight the hypothesis named H (asked interactively)."
  (interactive "sHypothesis name to highlight: ")
  (coq-hyps--toggle 'coq-highlighted-hyps (coq-hyps--names-of h) "highlighted"))

(defun coq-highlight-hyp (h)
  "Highlight the hypothesis named H."
  (interactive "sHypothesis name to highlight: ")
  (unless (member h coq-highlighted-hyps)
    (coq-toggle-highlight-hyp h)))

(defun coq-unhighlight-hyp (h)
  "Unhighlight the hypothesis named H."
  (interactive "sHypothesis name to unhighlight: ")
  (when (member h coq-highlighted-hyps)
    (coq-toggle-highlight-hyp h)))

(defun coq-unhighlight-selected-hyps ()
  "Unhighlight all the hypotheses."
  (interactive)
  (setq coq-highlighted-hyps nil)
  (coq-hyps-decorate))

(defun coq-highlight-hyps-cited-in-response ()
  "Highlight the hypotheses cited in the response buffer, by a Search.
See `coq-highlight-hyps-cited-in-response'."
  (let* ((response (and coq-response-buffer-function
                        (funcall coq-response-buffer-function)))
         (cited (and (buffer-live-p response)
                     (mapcan (lambda (position) (copy-sequence (car position)))
                             (coq-detect-hyps-positions response)))))
    (setq coq-highlighted-hyps
          (seq-filter (lambda (name) (member name cited)) (coq-hyps-names)))
    (coq-hyps-decorate)))


;;; Mouse

(defun coq-insert-at-point-hyp-at-mouse (event)
  "Insert at point the name of the hypothesis at mouse EVENT.
The name under the mouse if the hypothesis has several names."
  (interactive "e")
  (let* ((names (coq-hyps--at-mouse event))
         (clicked (with-current-buffer (window-buffer
                                        (posn-window (event-start event)))
                    (save-excursion
                      (goto-char (posn-point (event-start event)))
                      (ignore-errors (coq-id-at-point))))))
    (when names
      (insert (if (member clicked names) clicked (car names))))))

(define-key coq-hypname-map [mouse-3] #'coq-toggle-fold-hyp-at-mouse)
(define-key coq-hypname-map [mouse-2] #'coq-insert-at-point-hyp-at-mouse)
(define-key coq-hypcross-map [mouse-1] #'coq-toggle-fold-hyp-at-mouse)
(define-key coq-hypcross-map [return] #'coq-toggle-fold-hyp-at-point)
(define-key coq-hypcross-map [mouse-2] #'coq-insert-at-point-hyp-at-mouse)
(define-key coq-hidden-hyp-map [mouse-1] #'coq-toggle-fold-hyp-at-mouse)
(define-key coq-hidden-hyp-map [return] #'coq-toggle-fold-hyp-at-point)
(define-key coq-hidden-hyp-map [mouse-2] #'coq-insert-at-point-hyp-at-mouse)

(defun coq-hyps-define-keys (map)
  "Bind the keys of the hypotheses in MAP, the keymap of a goals buffer."
  (define-key map [?f] #'coq-toggle-fold-hyp-at-point)
  (define-key map [?F] #'coq-unfold-hyps)
  (define-key map [?h] #'coq-toggle-highlight-hyp-at-point)
  (define-key map [?H] #'coq-unhighlight-selected-hyps))

(defconst coq-hyps-menu
  '("Hypotheses"
    ["Toggle Folding" coq-toggle-fold-hyp-at-point]
    ["Unfold All" coq-unfold-hyps]
    ["Toggle Highlighting" coq-toggle-highlight-hyp-at-point]
    ["Unhighlight All" coq-unhighlight-selected-hyps]
    ["Sticky Highlighting" (setq coq-highlight-hyps-sticky
                                 (not coq-highlight-hyps-sticky))
     :style toggle :selected coq-highlight-hyps-sticky])
  "Menu of the hypotheses, for the menus of the backends.")


;;; Highlighting of the diffs, text marked with XML-like tags by Coq
;; Formerly coq-diffs.el.

(defun coq-highlight-with-face (end face)
  (if face
    ;; (put-text-property (point) end 'face face) doesn't work
    (overlay-put (span-make (point) end) 'face face))
  (goto-char end))

(defun coq-search (regex limit)
  (save-excursion
     (re-search-forward regex limit t)))

(defvar coq-removed-tags-regexp
  "<\\(?:infomsg\\|warning\\)>\n?\\|\n?</\\(?:infomsg\\|warning\\)>"
  "Regexp matching message tags to remove from text displayed to the user.
The newline following an opening tag or preceding a closing tag is
removed too, to avoid empty lines.")

(defun coq-insert-tagged-text (str)
  "Insert text into the current buffer applying faces specified by tags.

For example \\='<diff.added>foo</diff.added>\\=' inserts \\='foo\\=' in the buffer
and applies the appropriate face.

`coq-tag-map' defines the mapping from tag name to face.  Tags matching
`coq-removed-tags-regexp' are removed."
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
    (insert (replace-regexp-in-string coq-removed-tags-regexp "" str))
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

(provide 'coq-goals)
(provide 'coq-diffs)                    ; compatibility, see coq-diffs.el

;;; coq-goals.el ends here
