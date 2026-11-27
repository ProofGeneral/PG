;;; pg-output-windows.el --- Heights of the output windows  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 2026  Pierre Courtieu

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The goals and response windows one above the other share their
;; height, see `proof-shrink-windows-tofit': each one gets what it
;; needs if they all fit, otherwise the ones needing little get what
;; they need and the others share the rest, see
;; `proof-allocate-heights'.  The other windows, like the script
;; window, keep their height.  Then a response too long for its window
;; shows its end, see `proof-response-show-end'.
;;
;; A backend with other output windows passes them with their roles to
;; `proof-fit-output-windows', and may count the lines of its buffers
;; its own way, see `proof-window-lines-function'.

;;; Code:

(require 'cl-lib)
(require 'pg-vars)                      ; proof-goals-buffer
(require 'proof-useropts)

;;; Sharing the height

(defun proof--water-level (room excesses weights)
  "Level sharing ROOM lines between windows needing EXCESSES lines.
The windows have WEIGHTS: at level L, a window of excess E and weight
W gets min(E, W * L) lines, and they get ROOM lines in all.  Nil if
they all get their excess."
  (let ((windows (sort (cl-mapcar #'cons excesses weights)
                       (lambda (a b) (< (/ (car a) (float (cdr a)))
                                        (/ (car b) (float (cdr b)))))))
        (weight (apply #'+ weights))
        (level nil))
    (while (and windows (not level))
      (pcase-let ((`(,excess . ,window-weight) (car windows)))
        (if (<= (* excess weight) (* window-weight room))
            (setq room (- room excess)
                  weight (- weight window-weight)
                  windows (cdr windows))
          (setq level (/ room (float weight))))))
    level))

(defun proof-allocate-heights (total needs &optional weights minimum bonus)
  "Share TOTAL lines between windows needing NEEDS lines.
NEEDS lists the heights the windows need to display their whole
content, by priority.  WEIGHTS lists their weights, 1 by default.  Each
window gets at least MINIMUM lines, `window-min-height' by default.

If they all fit, each window gets what it needs, and the first one what
remains.  Otherwise the windows needing little get what they need, and
the others share the rest in proportion to their weights.  A window
needing at most BONUS (1 by default) times its share gets what it needs,
at the expense of the others that do not fit: displaying it entirely is
worth it.  These ones get the lines left by the rounding, the first
ones first.

Return the list of heights in the order of NEEDS, or nil if TOTAL is
smaller than MINIMUM lines per window."
  (let* ((minimum (or minimum window-min-height))
         (count (length needs))
         (weights (or weights (make-list count 1)))
         (bonus (or bonus 1))
         (room (- total (* minimum count)))
         (excesses (mapcar (lambda (need) (max 0 (- need minimum))) needs))
         (full nil)          ; the windows getting their excess with the bonus
         (level nil))
    (cl-labels ((open (list) (cl-loop for x in list for i from 0
                                      unless (memq i full) collect x))
                (left () (- room (apply #'+ (mapcar (lambda (i) (nth i excesses))
                                                    full))))
                (share (i) (* (nth i weights) level)))
      (when (and needs (>= room 0))
        (setq level (proof--water-level (left) (open excesses) (open weights)))
        ;; the bonus: one window at a time, the closest to its share
        (while
            (let* ((left (and level (left)))
                   (best (cl-loop
                          for i below count
                          for excess = (nth i excesses)
                          for ratio = (and left (/ (+ minimum excess)
                                                   (+ minimum (share i))))
                          when (and ratio (not (memq i full))
                                    (> excess (share i)) (<= ratio bonus)
                                    (<= excess left))
                          collect (cons ratio i))))
              (when best
                (push (cdar (sort best #'car-less-than-car)) full)
                (setq level (proof--water-level (left) (open excesses)
                                                (open weights)))
                t)))
        ;; rounded down for the windows that do not fit; they get the
        ;; lines left, otherwise the first window gets them
        (let* ((extras (cl-loop for i below count
                                for excess = (nth i excesses)
                                collect (if (or (memq i full) (not level)
                                                (<= excess (share i)))
                                            excess
                                          (floor (share i)))))
               (cut (cl-loop for i below count
                             unless (= (nth i extras) (nth i excesses))
                             collect i)))
          (dotimes (k (- room (apply #'+ extras)))
            (cl-incf (nth (if cut (nth (mod k (length cut)) cut) 0) extras)))
          (mapcar (lambda (extra) (+ minimum extra)) extras))))))

;;; Windows

(defvar proof-window-lines-function #'proof-window-lines
  "Function giving the number of lines a window needs for its buffer.
Called with a window displaying an output buffer, see
`proof-fit-output-windows'.")

(defun proof-window-lines (window)
  "Number of lines on screen of the buffer of WINDOW."
  (with-current-buffer (window-buffer window)
    (count-screen-lines (point-min) (point-max) nil window)))

(defun proof-window-need (window)
  "Height WINDOW needs to display its whole buffer, mode line included.
See `proof-window-lines-function'."
  (+ (- (window-total-height window) (window-body-height window))
     (funcall proof-window-lines-function window)))

(defun proof-output-window-weight (role)
  "Weight of the output windows of ROLE, see `proof-output-windows-weights'.
1 if it is not a positive number."
  (let ((weight (cdr-safe (ignore-errors
                            (assq role proof-output-windows-weights)))))
    (if (and (numberp weight) (> weight 0)) weight 1)))

(defun proof-output-window-columns (windows)
  "The WINDOWS adjacent one above the other, by column.
Lists of at least two windows, from top to bottom."
  (let ((columns nil))
    (dolist (window windows)
      (when (and (window-live-p window)
                 (window-combined-p window)
                 ;; the top one of its column
                 (not (memq (window-prev-sibling window) windows)))
        (let ((column nil))
          (while (memq window windows)
            (push window column)
            (setq window (window-next-sibling window)))
          (when (cdr column)
            (push (nreverse column) columns)))))
    columns))

(defun proof-resize-output-column (column roles)
  "Give the windows of COLUMN, one above the other, the height they need.
ROLES maps the windows to their roles, giving their weights.  The
windows of greater weight, then the upper ones, come first, see
`proof-allocate-heights'.  Only the edges between the windows move."
  (let* ((weight (lambda (window)
                   (proof-output-window-weight (cdr (assq window roles)))))
         ;; stable: the upper windows first at equal weight
         (by-priority (sort (copy-sequence column)
                            (lambda (a b) (> (funcall weight a)
                                             (funcall weight b)))))
         (heights (proof-allocate-heights
                   (apply #'+ (mapcar #'window-total-height column))
                   (mapcar #'proof-window-need by-priority)
                   (mapcar weight by-priority)
                   window-min-height
                   (if (and (numberp proof-shrink-windows-bonus)
                            (>= proof-shrink-windows-bonus 1))
                       proof-shrink-windows-bonus
                     1))))
    (when heights
      ;; from top to bottom: the last one gets what remains
      (dolist (window (butlast column))
        (let ((height (nth (cl-position window by-priority) heights)))
          (ignore-errors
            (adjust-window-trailing-edge
             window (- height (window-total-height window)))))))))

(defun proof-output-windows ()
  "The windows of the goals and response buffers, with their roles.
An alist from the windows to goals or response."
  (let ((windows nil))
    (dolist (role (list (cons 'goals proof-goals-buffer)
                        (cons 'response proof-response-buffer)))
      (when (buffer-live-p (cdr role))
        (dolist (window (get-buffer-window-list (cdr role) nil t))
          (push (cons window (car role)) windows))))
    (nreverse windows)))

(defun proof-fit-output-windows (&optional windows)
  "Fit the output windows to their contents, and show the responses.
WINDOWS is an alist from the output windows to their roles, the goals
and response windows by default, see `proof-output-windows'.  If
`proof-shrink-windows-tofit', the windows one above the other share
their height, see `proof-resize-output-column'.  Then the response
windows show the end of their response, see `proof-show-response'."
  (let ((windows (or windows (proof-output-windows))))
    (when (and proof-shrink-windows-tofit
               (not proof-multiple-frames-enable))
      (dolist (column (proof-output-window-columns (mapcar #'car windows)))
        (proof-resize-output-column column windows)))
    (dolist (window windows)
      (when (and (eq (cdr window) 'response) (window-live-p (car window)))
        (proof-show-response (car window))))))

(defun proof-show-response (window)
  "Scroll WINDOW to show its response buffer.
Its beginning if it fits, otherwise its end at the bottom of WINDOW,
or its beginning if `proof-response-show-end' is nil.  Point is at the
end of the response if visible, otherwise at its beginning."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (let ((end (point))
            (height (window-body-height window)))
        (cond
         ((<= (count-screen-lines (point-min) end nil window) height)
          (set-window-start window (point-min))
          (set-window-point window end))
         (proof-response-show-end
          (vertical-motion (- 1 height) window)
          (set-window-start window (point))
          (set-window-point window end))
         (t
          (set-window-start window (point-min))
          (set-window-point window (point-min))))))))

(provide 'pg-output-windows)

;;; pg-output-windows.el ends here
