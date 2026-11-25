;;; coq-test-output-windows.el --- Test the heights of the output windows  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the sharing of the height between the goals and response
;; windows one above the other (`proof-allocate-heights',
;; `proof-fit-output-windows'), the end of a long response
;; (`proof-response-show-end'), and that the old and wrong settings do
;; not signal errors.  Without Coq, except the last test: the end of a
;; long error message with Coq, in three windows mode.

;;; Code:

(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'ert)
(require 'pg-output-windows)
(require 'coq-goals)

;; let-bound in the tests: dynamic
(defvar proof-goals-buffer)
(defvar proof-response-buffer)
(defvar proof-shrink-windows-tofit)
(defvar proof-output-windows-weights)
(defvar proof-shrink-windows-bonus)
(defvar proof-response-show-end)
(defvar proof-multiple-frames-enable)
(defvar proof-window-lines-function)
(defvar coq-goals-buffer-function)
(defvar coq-optimise-resp-windows-enable)
(defvar proof-three-window-enable)
(defvar proof-three-window-mode-policy)
(defvar proof-second-action-list-active)
(defvar proof-action-list)

(declare-function proof-shell-exit "proof-shell")

;;; Sharing the height, without windows

(ert-deftest output-windows-allocate ()
  "The heights of the windows, with the bonus of 1.5."
  (cl-flet ((allocate (total needs &optional weights)
              (proof-allocate-heights total needs weights 4 1.5)))
    ;; they all fit: the first one gets what remains
    (should (equal (allocate 40 '(10 12)) '(28 12)))
    ;; the response does not fit: the goals get what they need
    (should (equal (allocate 40 '(10 100)) '(10 30)))
    ;; the goals need a bit more than their share: they get it
    (should (equal (allocate 45 '(5 30 200)) '(5 30 10)))
    ;; two windows too large share what remains
    (should (equal (allocate 45 '(5 60 50)) '(5 20 20)))
    ;; three windows too large share it all
    (should (equal (allocate 45 '(30 30 30)) '(15 15 15)))
    ;; a window of weight 2 gets twice as much, here with the bonus
    (should (equal (allocate 45 '(30 30 30) '(2 1 1)) '(30 8 7)))
    ;; the minimum
    (should (equal (allocate 12 '(1 1 100)) '(4 4 4)))
    (should-not (allocate 11 '(1 1 100)))))

(ert-deftest output-windows-allocate-no-bonus ()
  "Without bonus, the windows that do not fit share fairly."
  (should (equal (proof-allocate-heights 45 '(5 30 200) nil 4 1)
                 '(5 20 20))))

(ert-deftest output-windows-allocate-total ()
  "The heights always sum up to the total, above the minimum."
  (dotimes (_ 500)
    (let* ((count (1+ (random 4)))
           (total (+ (* 4 count) (random 80)))
           (needs (mapcar (lambda (_) (random 100)) (make-list count nil)))
           (weights (mapcar (lambda (_) (1+ (random 3))) needs))
           (heights (proof-allocate-heights total needs weights 4 1.5)))
      (should (= (apply #'+ heights) total))
      (should (cl-every (lambda (height) (>= height 4)) heights)))))

;;; Windows

(defmacro output-windows-test (goals response &rest body)
  "Run BODY with three windows: script, GOALS and RESPONSE, one above the other.
GOALS and RESPONSE are the numbers of lines of the buffers."
  (declare (indent 2))
  `(let ((proof-goals-buffer (get-buffer-create "*test goals*"))
         (proof-response-buffer (get-buffer-create "*test response*"))
         (proof-shrink-windows-tofit t)
         (proof-output-windows-weights '((goals . 1) (response . 1)))
         (proof-shrink-windows-bonus 1.5)
         (proof-response-show-end t)
         (proof-multiple-frames-enable nil))
     (unwind-protect
         (save-window-excursion
           ;; some Emacs versions run with a frame of 10x9 in batch mode
           (set-frame-height (selected-frame) 40)
           (set-frame-width (selected-frame) 80)
           (delete-other-windows)
           (let* ((script (selected-window))
                  (goals-window (split-window script 16))
                  (response-window (split-window goals-window 12)))
             (dolist (b (list (cons proof-goals-buffer ,goals)
                              (cons proof-response-buffer ,response)))
               (with-current-buffer (car b)
                 (erase-buffer)
                 (dotimes (i (cdr b)) (insert (format "line %d\n" i)))))
             (set-window-buffer goals-window proof-goals-buffer)
             (set-window-buffer response-window proof-response-buffer)
             (ignore script goals-window response-window)
             ,@body))
       (kill-buffer proof-goals-buffer)
       (kill-buffer proof-response-buffer))))

(defun output-windows-end-visible-p (window)
  "Non-nil if the last line of the buffer of WINDOW is at its bottom."
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (= (count-screen-lines (window-start window) (point) nil window)
         (window-body-height window)))))

(ert-deftest output-windows-fit-column ()
  "The goals and response share their height, the script keeps its own."
  (output-windows-test 5 100
    (let ((script-height (window-total-height script))
          (total (+ (window-total-height goals-window)
                    (window-total-height response-window))))
      (proof-fit-output-windows)
      (should (= (window-total-height script) script-height))
      ;; the goals need their lines and a mode line
      (should (= (window-total-height goals-window)
                 (+ 5 (- (window-total-height goals-window)
                         (window-body-height goals-window)))))
      (should (= (+ (window-total-height goals-window)
                    (window-total-height response-window))
                 total))
      ;; the response does not fit: its end is visible
      (should (output-windows-end-visible-p response-window))
      (should (> (window-start response-window) 1)))))

(ert-deftest output-windows-response-beginning ()
  "With `proof-response-show-end' nil, the response shows its beginning."
  (output-windows-test 5 100
    (let ((proof-response-show-end nil))
      (proof-fit-output-windows)
      (should (= (window-start response-window) 1)))))

(ert-deftest output-windows-no-resize ()
  "Without `proof-shrink-windows-tofit', the heights do not change."
  (output-windows-test 5 100
    (let ((proof-shrink-windows-tofit nil)
          (heights (mapcar #'window-total-height (window-list))))
      (proof-fit-output-windows)
      (should (equal (mapcar #'window-total-height (window-list)) heights))
      ;; the response still shows its end
      (should (output-windows-end-visible-p response-window)))))

(ert-deftest output-windows-wrong-settings ()
  "Wrong settings use the defaults instead of signaling errors."
  (dolist (weights '(foo ((goals . "big")) ((goals . -1)) (1 2)))
    (output-windows-test 30 30
      (let ((proof-output-windows-weights weights)
            (proof-shrink-windows-bonus 'big))
        (proof-fit-output-windows)
        ;; equal weights: the rounding gives the goals one line more
        (should (<= 0 (- (window-total-height goals-window)
                         (window-total-height response-window))
                    1))))))

(ert-deftest output-windows-old-option ()
  "The option of Coq is an alias of `proof-shrink-windows-tofit'."
  (let ((proof-shrink-windows-tofit t))
    (setq coq-optimise-resp-windows-enable nil)
    (should-not proof-shrink-windows-tofit)))

(ert-deftest output-windows-coq-other-goals ()
  "In Coq, the goals after the first one count for one line."
  (output-windows-test 0 0
    (let ((coq-goals-buffer-function (lambda () proof-goals-buffer)))
      (with-current-buffer proof-goals-buffer
        (insert "x : nat\n============================\nx = x\n\n"
                "goal 2 is:\n y = y\n\ngoal 3 is:\n z = z\n"))
      (should (= (coq-goals-window-lines goals-window) 5))
      ;; not in the response
      (with-current-buffer proof-response-buffer
        (insert "goal 2 is:\n y = y\n"))
      (should (= (coq-goals-window-lines response-window) 2)))))

;;; With Coq

(defconst output-windows-error-src
  (concat "Lemma long : "
          (mapconcat (lambda (i) (format "%d = %d -> " i i))
                     (number-sequence 1 40) "")
          "True.\n"
          "Proof.\n"
          "  intros.\n"
          "  exact 0. (* marker *)\n"
          "Abort.\n")
  "An error whose message begins with a long context.")

(ert-deftest output-windows-coq-error-end ()
  "The response window shows the end of a long error message."
  (let ((proof-three-window-enable t)
        (proof-three-window-mode-policy 'vertical)
        (file (make-temp-file "output_windows" nil ".v"
                              output-windows-error-src))
        buffer)
    (set-frame-height (selected-frame) 40)
    (set-frame-width (selected-frame) 80)
    (unwind-protect
        (progn
          (find-file file)
          (setq buffer (current-buffer))
          (goto-char (point-min))
          (should (search-forward "(* marker *)" nil t))
          (proof-goto-point)
          (while (or proof-second-action-list-active
                     (consp proof-action-list))
            (accept-process-output nil 0.1))
          (let ((window (get-buffer-window proof-response-buffer)))
            (should window)
            (with-current-buffer proof-response-buffer
              (should (string-match-p "In environment" (buffer-string)))
              (goto-char (point-max))
              (skip-chars-backward " \t\n")
              ;; the context does not fit, the error is visible
              (should (> (window-start window) (point-min)))
              (should (= (count-screen-lines (window-start window) (point)
                                             nil window)
                         (window-body-height window)))
              (should (re-search-backward "expected to have type"
                                          (window-start window) t)))))
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (proof-shell-live-buffer)
        (proof-shell-exit t))
      (delete-file file))))

(provide 'coq-test-output-windows)

;;; coq-test-output-windows.el ends here
