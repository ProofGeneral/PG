;;; pg-goals-indicators.el --- Tell when the goals continue out of view  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; In the windows of the goals buffer, arrows in the fringe tell when
;; the goals continue above or below, see `indicate-buffer-boundaries'.
;; Scrolling stops when the end of the goals reaches the bottom of the
;; window: scrolling further would only hide them.
;;
;; This file depends neither on the prover nor on the proof shell: a
;; goals buffer calls `proof-goals-indicators-setup'.

;;; Code:

(require 'proof-useropts)

(defcustom proof-goals-indicators-enable t
  "*Whether goals windows tell when the goals continue above or below.
Arrows are then displayed in the left fringe, see
`indicate-buffer-boundaries'.  They are not visible if the fringe is
too narrow, see `fringe-mode'."
  :type 'boolean
  :group 'proof-user-options)

(defcustom proof-goals-scroll-margin 1
  "*Number of empty lines that scrolling can show after the goals.
Scrolling further would only hide them.  If nil, scrolling is not
limited."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'proof-user-options)

(defun proof-goals-indicators-setup ()
  "Set up the indicators and the scrolling of the current goals buffer."
  (when proof-goals-indicators-enable
    (setq-local indicate-buffer-boundaries '((up . left) (down . left))))
  (add-hook 'window-scroll-functions #'proof-goals--scrolled nil t))

(defun proof-goals--max-window-start (window)
  "Last start of WINDOW still showing the end of its buffer.
With `proof-goals-scroll-margin' empty lines after it, but no scrolling
at all if the whole buffer fits."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (let ((last (point))
            (height (window-body-height window)))
        (goto-char (point-min))
        (if (and (= (vertical-motion height window) height)
                 (<= (point) last))
            (progn
              (goto-char last)
              (vertical-motion (- (1+ proof-goals-scroll-margin) height)
                               window)
              (point))
          (point-min))))))

(defun proof-goals--scrolled (window start)
  "Keep the end of the goals in WINDOW, scrolled to START.
See `proof-goals-scroll-margin'.  For `window-scroll-functions'."
  (when proof-goals-scroll-margin
    (let ((max (proof-goals--max-window-start window)))
      (when (> start max)
        (set-window-start window max)))))

(provide 'pg-goals-indicators)

;;; pg-goals-indicators.el ends here
