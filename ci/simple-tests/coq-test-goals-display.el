;;; coq-test-goals-display.el --- Test the display of the goals  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test which part of the goals is visible when they do not fit in
;; their window, the indicators of what is out of view, the number of
;; goals in the mode line and the folding of the goals after the first
;; one.

;;; Code:

(setq proof-three-window-enable t)
;; Some Emacs versions run with a frame width of 10 in batch mode
;; inside the container. The line breaks make some regular expressions
;; fail, therfore disable printing width adapting.
(setq coq-auto-adapt-printing-width nil)

(require 'proof-site)
(proof-ready-for-assistant 'coq)

(require 'ert)

;; let-bound in the tests: dynamic, even before coq.el is loaded
(defvar coq-prefer-top-of-conclusion)
(defvar coq-user-init-cmd)
(defvar coq-fold-other-goals)
(defvar proof-second-action-list-active)
(defvar proof-action-list)
(defvar proof-goals-buffer)
(defvar proof-shell-active-scripting-indicator)

(declare-function coq-goals-folded-start "coq-goals")
(declare-function coq-goals-mode-line "coq-goals")
(declare-function coq-toggle-other-goals "coq-goals")
(declare-function proof-goals--scrolled "pg-goals-indicators")
(declare-function proof-goals--max-window-start "pg-goals-indicators")
(declare-function proof-shell-exit "proof-shell")


;;; Coq source code for tests

(defconst coq-src-goals-display
  (concat
   "Lemma many : "
   (mapconcat (lambda (i) (format "%d = %d -> " i i)) (number-sequence 1 30) "")
   "True.\n"
   "Proof.\n"
   "  intros. (* marker many *)\n"
   "Abort.\n\n"
   "Lemma long : "
   (mapconcat (lambda (i) (format "%d = %d -> " i i)) (number-sequence 1 10) "")
   "\n  "
   (mapconcat (lambda (i) (format "%d = %d" i i)) (number-sequence 1 150) " /\\ ")
   ".\n"
   "Proof.\n"
   "  intros. (* marker long *)\n"
   "Abort.\n\n"
   "Lemma split3 : forall n, n + 0 = n /\\ True /\\ False.\n"
   "Proof.\n"
   "  intros n. split; [|split]. (* marker split *)\n"
   "  - (* marker bullet *)\n"
   "Abort.\n\n"
   "Lemma closed : True /\\ True /\\ True.\n"
   "Proof.\n"
   "  split; [|split].\n"
   "  - exact I. (* marker closed *)\n"
   "Abort.\n\n"
   "Lemma manysplit : "
   (mapconcat (lambda (i) (format "%d = %d -> " i i)) (number-sequence 1 30) "")
   "True /\\ True.\n"
   "Proof.\n"
   "  intros. split. (* marker manysplit *)\n"
   "Abort.\n\n"
   "Lemma small : True.\n"
   "Proof. (* marker small *)\n"
   "Abort.\n")
  "Coq source code for the tests of the goals display.")


;;; utility functions

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    (accept-process-output nil 0.1)))

(defmacro with-goals-at-marker (marker &rest body)
  "Process the test source up to MARKER, then run BODY in the goals buffer.
The goals window is bound to `window'."
  (declare (indent 1) (debug t))
  `(let ((file (make-temp-file "goals_display" nil ".v" coq-src-goals-display))
         buffer)
     (unwind-protect
         (progn
           (find-file file)
           (setq buffer (current-buffer))
           (goto-char (point-min))
           (should (search-forward (format "(* marker %s *)" ,marker) nil t))
           (goto-char (match-beginning 0))
           (proof-goto-point)
           (wait-for-coq)
           (let ((window (get-buffer-window proof-goals-buffer)))
             (should window)
             (with-current-buffer proof-goals-buffer
               ,@body)))
       (when buffer
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (when (proof-shell-live-buffer)
         (proof-shell-exit t))
       (delete-file file))))

(defun window-last-position (window)
  "Position after the last line displayed in WINDOW."
  (save-excursion
    (goto-char (window-start window))
    (vertical-motion (window-body-height window) window)
    (point)))

(defun separator-line ()
  "Start of the line of the separator before the conclusion."
  (save-excursion
    (goto-char (point-min))
    (should (re-search-forward "=====" nil t))
    (line-beginning-position)))


(defun mode-line-goals ()
  "The part of the mode line of the current goals buffer about the goals.
`format-mode-line' returns nothing in batch mode."
  (should (equal mode-line-process '(:eval (coq-goals-mode-line))))
  (coq-goals-mode-line))

(defun other-goals-overlay ()
  "The overlay of the goals after the first one, or nil."
  (seq-find (lambda (ov) (overlay-get ov 'coq-other-goals))
            (overlays-in (point-min) (point-max))))


;;; tests

(ert-deftest goals-display-last-hyps ()
  "When the goals do not fit, the last hypotheses and the conclusion are
displayed."
  (with-goals-at-marker "many"
    (should (> (window-start window) (point-min)))
    (should (>= (window-last-position window)
                (save-excursion (goto-char (point-max))
                                (skip-chars-backward " \n")
                                (point))))))

(ert-deftest goals-display-prefer-top-of-conclusion ()
  "With `coq-prefer-top-of-conclusion', a goal that does not fit shows
its separator and its beginning."
  (let ((coq-prefer-top-of-conclusion t))
    (with-goals-at-marker "long"
      (should (= (window-start window) (separator-line)))))
  (let ((coq-prefer-top-of-conclusion nil))
    (with-goals-at-marker "long"
      (should (> (window-start window) (separator-line))))))

(ert-deftest goals-display-point-elsewhere ()
  "The first goal is shown wherever point is in the goals buffer, and
point does not move if it stays in view."
  (with-goals-at-marker "many"
    (let ((start (window-start window))
          (pos (save-excursion (goto-char (point-max))
                               (skip-chars-backward " \n")
                               (line-beginning-position 0))))
      ;; point near the conclusion, the window scrolled to the top
      (set-window-point window pos)
      (goto-char pos)
      (set-window-start window (point-min))
      (coq-show-first-goal)
      (should (= (window-start window) start))
      (should (= (window-point window) pos)))))

(ert-deftest goals-display-subproof-complete-top ()
  "When no goal is focused, the goals are shown from the top."
  (with-goals-at-marker "closed"
    (should (= (window-start window) (point-min)))))

(ert-deftest goals-display-indicators ()
  "Arrows in the fringe tell when the goals continue above or below.
Scrolling stops at the end of the goals."
  (with-goals-at-marker "many"
    (should (equal indicate-buffer-boundaries '((up . left) (down . left))))
    ;; scrolling stops at the end, with an empty line after it
    (let ((max (proof-goals--max-window-start window)))
      (should (> max (point-min)))
      (proof-goals--scrolled window (1- (point-max)))
      (should (= (window-start window) max))
      (should (>= (window-last-position window)
                  (save-excursion (goto-char (point-max))
                                  (skip-chars-backward " \n")
                                  (point)))))))

(ert-deftest goals-display-fits ()
  "When the goals fit: no scrolling."
  ;; For Emacs 26.3, 27.1, and 27.2, the default frame is 10x9 when
  ;; running in a docker container (see coq-test-three-window.el): the
  ;; goals window has 10 columns and 3 lines and the goals cannot fit.
  (skip-unless (version<= "28" emacs-version))
  (with-goals-at-marker "small"
    (proof-goals--scrolled window (1- (point-max)))
    (should (= (window-start window) (point-min)))))

(ert-deftest goals-display-fold-other-goals ()
  "The goals after the first one are folded to their number.  RET or a
click on the line unfolds them."
  (unwind-protect
      (with-goals-at-marker "split"
        (let ((ov (other-goals-overlay)))
          (should ov)
          (should (equal (substring-no-properties (overlay-get ov 'display))
                         "▸ 2 more goals"))
          (should (= (overlay-start ov) (coq-goals-folded-start)))
          ;; the text is still there
          (should (string-match-p "goal 3 (ID [0-9]+) is:" (buffer-string))))
        (goto-char (coq-goals-folded-start))
        (should (eq (key-binding (kbd "RET")) #'coq-toggle-other-goals))
        (call-interactively (key-binding (kbd "RET")))
        (should-not coq-fold-other-goals)
        (let ((ov (other-goals-overlay)))
          (should-not (overlay-get ov 'display))
          (should (equal (substring-no-properties (overlay-get ov 'before-string))
                         "▾ 2 more goals\n")))
        (should-not (coq-goals-folded-start)))
    (setq coq-fold-other-goals t)))

(ert-deftest goals-display-unfocused-goals ()
  "With Set Printing Unfocused, the unfocused goals are counted."
  (let ((coq-user-init-cmd '("Set Printing Unfocused.")))
    (with-goals-at-marker "bullet"
      (should (equal (substring-no-properties
                      (overlay-get (other-goals-overlay) 'display))
                     "▸ 2 unfocused goals"))
      (should (equal (mode-line-goals) " 1+2"))
      (should (eq (get-text-property 1 'face (mode-line-goals))
                  'coq-goals-count-face))
      (should (equal (get-text-property 1 'help-echo (mode-line-goals))
                     "Goals: 1 focused, 2 unfocused")))))

(ert-deftest goals-display-subproof-complete ()
  "When the focused goal is closed, the goals left are unfocused."
  (let ((coq-user-init-cmd '("Set Printing Unfocused.")))
    (with-goals-at-marker "closed"
      (should (equal (mode-line-goals) " 0+2"))
      (should (equal (substring-no-properties
                      (overlay-get (other-goals-overlay) 'display))
                     "▸ 2 unfocused goals"))))
  ;; here Coq displays them even without Set Printing Unfocused
  (with-goals-at-marker "closed"
    (should (equal (mode-line-goals) " 0+2"))))

(ert-deftest goals-display-mode-line ()
  "The mode line of the goals buffer gives the number of goals, not the
one of the script buffer.  Set Printing Unfocused is supposed to be on:
no unfocused goal displayed means none."
  (with-goals-at-marker "split"
    (should (equal (mode-line-goals) " 3+0"))
    (should (equal (get-text-property 1 'help-echo (mode-line-goals))
                   "Goals: 3 focused, 0 unfocused"))
    ;; the script buffer keeps the generic indicator
    (should (eq (cadr (assq 'proof-active-buffer-fake-minor-mode
                            minor-mode-alist))
                proof-shell-active-scripting-indicator))))

(ert-deftest goals-display-folded-line-last ()
  "The line of the folded goals is not worth a line of the first goal:
the end of the conclusion is at the bottom of the window."
  (with-goals-at-marker "manysplit"
    (should (> (window-start window) (point-min)))
    (should (>= (coq-goals-folded-start) (window-last-position window)))
    (should (< (save-excursion (goto-char (coq-goals-folded-start))
                               (skip-chars-backward " \n")
                               (point))
               (window-last-position window)))))

(provide 'coq-test-goals-display)

;;; coq-test-goals-display.el ends here
