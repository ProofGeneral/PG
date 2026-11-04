;;; coq-hyps.el --- Folding and highlighting of the hypotheses  -*- lexical-binding: t -*-

;; This file is part of Proof General.

;; Portions © Copyright 2001-2017, 2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
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

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'coq-db)                       ; faces of the buttons
(require 'coq-syntax)                   ; coq-detect-hyps-positions
(require 'coq-goals)                    ; coq-goals-buffer-function

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

(defvar coq-response-buffer-function nil
  "Function returning the response buffer of the current backend.")

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

(provide 'coq-hyps)

;;; coq-hyps.el ends here
