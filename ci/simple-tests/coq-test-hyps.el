;;; coq-test-hyps.el --- Test the folding and highlighting of hypotheses  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the folding and highlighting of the hypotheses in the goals
;; buffer: by keys, by name, by mouse, kept in the next goals, and the
;; highlighting of the hypotheses cited by a Search.

;;; Code:

(setq proof-three-window-enable t)
;; narrow batch frames: no line breaks in the goals
(setq coq-auto-adapt-printing-width nil)

(require 'proof-site)
(proof-ready-for-assistant 'coq)

(require 'ert)
(require 'cl-lib)

(defvar coq-highlight-hyps-sticky)
(defvar coq-hidden-hyps)
(defvar coq-highlighted-hyps)
(defvar proof-second-action-list-active)
(defvar proof-action-list)
(defvar proof-goals-buffer)
(defvar proof-response-buffer)

(declare-function proof-shell-exit "proof-shell")
(declare-function coq-run-query "coq-queries")
(declare-function coq-search-done "coq-queries")
(declare-function coq-toggle-fold-hyp-at-point "coq-goals")
(declare-function coq-toggle-fold-hyp-at-mouse "coq-goals")
(declare-function coq-toggle-fold-hyp "coq-goals")
(declare-function coq-unfold-hyps "coq-goals")
(declare-function coq-toggle-highlight-hyp-at-point "coq-goals")
(declare-function coq-toggle-highlight-hyp "coq-goals")
(declare-function coq-unhighlight-selected-hyps "coq-goals")
(declare-function coq-insert-at-point-hyp-at-mouse "coq-goals")


;;; utility functions

(defconst coq-test-hyps-src
  (concat
   "Lemma l (n m : nat) (H : n = m) (Hd := 3) : m = n.\n"
   "Proof. (* marker proof *)\n"
   "  assert (H2 : True) by trivial. (* marker assert *)\n"
   "Abort.\n"))

(defun coq-test-hyps-wait ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    (accept-process-output nil 0.1)))

(defun coq-test-hyps-goto (marker)
  "Process the script up to the end of the line of MARKER."
  (goto-char (point-min))
  (search-forward (format "(* marker %s *)" marker))
  (end-of-line)
  (proof-goto-point)
  (coq-test-hyps-wait))

(defmacro coq-test-hyps-with-file (&rest body)
  "Run BODY in a buffer visiting the Coq source, processed to the proof."
  (declare (indent 0) (debug t))
  `(let ((file (make-temp-file "hyps" nil ".v" coq-test-hyps-src))
         buffer)
     (unwind-protect
         (progn
           (find-file file)
           (setq buffer (current-buffer))
           (coq-test-hyps-goto "proof")
           ,@body)
       (ignore-errors (coq-unfold-hyps))
       (ignore-errors (coq-unhighlight-selected-hyps))
       (when buffer
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (when (proof-shell-live-buffer)
         (proof-shell-exit t))
       (delete-file file))))

(defun coq-test-hyps-type-position (name)
  "Position of the type of hypothesis NAME in the goals buffer."
  (with-current-buffer proof-goals-buffer
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^  \\(?:[^ \n]+, \\)*" (regexp-quote name)
                     "\\(?:, [^ \n]+\\)* :=? ")
             nil t)
        (point)))))

(defun coq-test-hyps-overlay (name property)
  "An overlay with PROPERTY on the type of hypothesis NAME, or nil."
  (let ((pos (coq-test-hyps-type-position name)))
    (should pos)
    (with-current-buffer proof-goals-buffer
      (seq-find (lambda (ov) (overlay-get ov property))
                (overlays-at pos)))))

(defun coq-test-hyps-folded-p (name)
  "Non-nil if the type of hypothesis NAME is hidden by dots."
  (let ((ov (coq-test-hyps-overlay name 'display)))
    (and ov (string-match-p "\\`\\.+\\'" (overlay-get ov 'display)))))

(defun coq-test-hyps-highlighted-p (name)
  "Non-nil if hypothesis NAME is highlighted."
  (coq-test-hyps-overlay name 'face))

(defun coq-test-hyps-cross (name)
  "The text displayed by the folding button of hypothesis NAME."
  (with-current-buffer proof-goals-buffer
    (save-excursion
      (goto-char (coq-test-hyps-type-position name))
      (let ((ov (seq-find (lambda (ov) (overlay-get ov 'display))
                          (overlays-at (line-beginning-position)))))
        (and ov (overlay-get ov 'display))))))

(defun coq-test-hyps-in-goals (function name)
  "Call FUNCTION with point on the name of hypothesis NAME in the goals."
  (with-current-buffer proof-goals-buffer
    (goto-char (coq-test-hyps-type-position name))
    (search-backward name)
    (funcall function)))

(defun coq-test-hyps-click (function name)
  "Call FUNCTION with a mouse event on the name of hypothesis NAME."
  (let ((pos (with-current-buffer proof-goals-buffer
               (goto-char (coq-test-hyps-type-position name))
               (search-backward name))))
    (funcall function
             `(mouse-1 (,(get-buffer-window proof-goals-buffer) ,pos (0 . 0) 0)))))


;;; tests

(ert-deftest coq-test-hyps-keys ()
  "The keys of the goals buffer for hypotheses."
  (coq-test-hyps-with-file
    (with-current-buffer proof-goals-buffer
      (should (eq (key-binding "f") #'coq-toggle-fold-hyp-at-point))
      (should (eq (key-binding "F") #'coq-unfold-hyps))
      (should (eq (key-binding "h") #'coq-toggle-highlight-hyp-at-point))
      (should (eq (key-binding "H") #'coq-unhighlight-selected-hyps)))))

(ert-deftest coq-test-hyps-fold ()
  "A folded hypothesis shows dots instead of its type, and stays folded
in the next goals.  Hypotheses on the same line fold together."
  (coq-test-hyps-with-file
    (should (equal (coq-test-hyps-cross "H") "-"))
    (coq-test-hyps-in-goals #'coq-toggle-fold-hyp-at-point "H")
    (should (coq-test-hyps-folded-p "H"))
    (should (equal (coq-test-hyps-cross "H") "+"))
    (should-not (coq-test-hyps-folded-p "n"))
    ;; the text is still there, for copy
    (should (string-match-p "H : n = m"
                            (with-current-buffer proof-goals-buffer
                              (buffer-string))))
    (coq-test-hyps-goto "assert")
    (should (coq-test-hyps-folded-p "H"))
    (coq-test-hyps-in-goals #'coq-toggle-fold-hyp-at-point "m")
    (should (coq-test-hyps-folded-p "n"))
    (should (coq-test-hyps-folded-p "m"))
    ;; a definition
    (coq-test-hyps-in-goals #'coq-toggle-fold-hyp-at-point "Hd")
    (should (coq-test-hyps-folded-p "Hd"))
    ;; toggle again
    (coq-test-hyps-in-goals #'coq-toggle-fold-hyp-at-point "H")
    (should-not (coq-test-hyps-folded-p "H"))
    (should (equal (coq-test-hyps-cross "H") "-"))
    (coq-unfold-hyps)
    (should-not coq-hidden-hyps)
    (should-not (coq-test-hyps-folded-p "n"))
    (should-not (coq-test-hyps-folded-p "Hd"))))

(ert-deftest coq-test-hyps-fold-by-name ()
  "`coq-toggle-fold-hyp' folds a hypothesis given by its name."
  (coq-test-hyps-with-file
    (coq-toggle-fold-hyp "H")
    (should (coq-test-hyps-folded-p "H"))
    (coq-toggle-fold-hyp "H")
    (should-not (coq-test-hyps-folded-p "H"))))

(ert-deftest coq-test-hyps-from-script ()
  "In the script, the commands work on the hypothesis named at point."
  (coq-test-hyps-with-file
    (goto-char (point-min))
    (search-forward "(H ")
    (backward-char 2)
    (coq-toggle-fold-hyp-at-point)
    (should (coq-test-hyps-folded-p "H"))
    (coq-toggle-highlight-hyp-at-point)
    (should (coq-test-hyps-highlighted-p "H"))
    (search-backward "nat")
    (should-error (coq-toggle-fold-hyp-at-point) :type 'user-error)))

(ert-deftest coq-test-hyps-fold-by-mouse ()
  "A click on the button of a hypothesis folds or unfolds it."
  (coq-test-hyps-with-file
    (coq-test-hyps-click #'coq-toggle-fold-hyp-at-mouse "H")
    (should (coq-test-hyps-folded-p "H"))
    (coq-test-hyps-click #'coq-toggle-fold-hyp-at-mouse "H")
    (should-not (coq-test-hyps-folded-p "H"))))

(ert-deftest coq-test-hyps-insert-by-mouse ()
  "A middle click on a hypothesis inserts its name at point."
  (coq-test-hyps-with-file
    (with-temp-buffer
      (coq-test-hyps-click #'coq-insert-at-point-hyp-at-mouse "H")
      (should (equal (buffer-string) "H")))))

(ert-deftest coq-test-hyps-highlight ()
  "Highlighting a hypothesis, kept in the next goals only if
`coq-highlight-hyps-sticky'."
  (coq-test-hyps-with-file
    (coq-test-hyps-in-goals #'coq-toggle-highlight-hyp-at-point "H")
    (should (coq-test-hyps-highlighted-p "H"))
    (should-not (coq-test-hyps-highlighted-p "n"))
    (coq-test-hyps-in-goals #'coq-toggle-highlight-hyp-at-point "H")
    (should-not (coq-test-hyps-highlighted-p "H"))
    (coq-toggle-highlight-hyp "H")
    (should (coq-test-hyps-highlighted-p "H"))
    (coq-unhighlight-selected-hyps)
    (should-not (coq-test-hyps-highlighted-p "H"))
    (should-not coq-highlighted-hyps)
    ;; not sticky: forgotten in the next goals
    (coq-toggle-highlight-hyp "H")
    (coq-test-hyps-goto "assert")
    (should-not (coq-test-hyps-highlighted-p "H"))
    (should-not coq-highlighted-hyps)
    (let ((coq-highlight-hyps-sticky t))
      (coq-test-hyps-goto "proof")
      (coq-toggle-highlight-hyp "H")
      (coq-test-hyps-goto "assert")
      (should (coq-test-hyps-highlighted-p "H"))
      (coq-unhighlight-selected-hyps))))

(ert-deftest coq-test-hyps-search ()
  "A Search highlights the hypotheses it finds."
  (coq-test-hyps-with-file
    (coq-run-query "Search (_ = _)" nil #'coq-search-done)
    (coq-test-hyps-wait)
    (should (string-match-p "^H: n = m"
                            (with-current-buffer proof-response-buffer
                              (buffer-string))))
    (should (coq-test-hyps-highlighted-p "H"))
    (should-not (coq-test-hyps-highlighted-p "n"))
    (should (equal coq-highlighted-hyps '("H")))))

(provide 'coq-test-hyps)

;;; coq-test-hyps.el ends here
