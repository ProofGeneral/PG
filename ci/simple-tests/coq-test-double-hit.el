;;; coq-test-double-hit.el --- Test the double hit electric terminator  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test that, with the double hit enabled, hitting "." twice quickly
;; processes the script up to it, and that it excludes the electric
;; terminator.  Test also the default implementation shared by all
;; proof assistants, including the unchanged electric behaviour.

;;; Code:

(setq proof-three-window-enable nil)

(require 'cl-lib)
(require 'proof-site)
(proof-ready-for-assistant 'coq)

(defvar proof-electric-terminator-enable)
(defvar proof-double-hit-enable)
(defvar proof-double-hit--time)
(declare-function proof-electric-terminator "pg-terminator")
(declare-function proof-double-hit-toggle "pg-terminator")


;;; utility functions

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    (accept-process-output nil 0.1)))

(defun type-key (char)
  "Type CHAR in the current buffer, as if with the keyboard."
  (let ((last-command-event char))
    (call-interactively (key-binding (string char)))))

(defun type-dot ()
  "Type \".\" in the current buffer, as if with the keyboard."
  (type-key ?.))

(defmacro with-double-hit-file (&rest body)
  "Run BODY in a buffer visiting a Coq file ending with a sentence
without its final dot, at the end of the buffer."
  (declare (indent 0) (debug t))
  `(let ((file (make-temp-file "double_hit" nil ".v"
                               "Definition x := 0.\nDefinition y := 1"))
         buffer)
     (unwind-protect
         (progn
           (find-file file)
           (setq buffer (current-buffer))
           (goto-char (point-max))
           ,@body)
       (when buffer
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (when (proof-shell-live-buffer)
         (proof-shell-exit t))
       (delete-file file))))


;;; tests

(ert-deftest proof-electric-terminator-unchanged ()
  "The default electric terminator keeps its former behaviour."
  (with-temp-buffer
    (let ((proof-electric-terminator-enable t)
          (proof-double-hit-enable nil)
          (inside-comment nil)
          (inside-string nil)
          (asserted 0)
          (last-command-event ?.))
      (cl-letf (((symbol-function 'proof-inside-comment)
                 (lambda (_pos) inside-comment))
                ((symbol-function 'proof-inside-string)
                 (lambda (_pos) inside-string))
                ((symbol-function 'proof-assert-electric-terminator)
                 (lambda () (setq asserted (1+ asserted)))))
        ;; Outside comments and strings it processes without inserting.
        (proof-electric-terminator)
        (should (= asserted 1))
        (should (equal (buffer-string) ""))
        ;; Inside either one it only inserts the terminator.
        (setq inside-comment t)
        (proof-electric-terminator)
        (should (= asserted 1))
        (should (equal (buffer-string) "."))
        (setq inside-comment nil inside-string t)
        (proof-electric-terminator)
        (should (= asserted 1))
        (should (equal (buffer-string) ".."))
        ;; A prefix argument still bypasses the electric action.
        (setq inside-string nil)
        (proof-electric-terminator 3)
        (should (= asserted 1))
        (should (equal (buffer-string) "....."))))))

(ert-deftest proof-double-hit-default-implementation ()
  "The default Proof General terminator implements the double hit."
  (with-temp-buffer
    (let ((proof-electric-terminator-enable nil)
          (proof-double-hit-enable t)
          (proof-double-hit--time nil)
          (asserted 0)
          (last-command-event ?.))
      (cl-letf (((symbol-function 'proof-assert-electric-terminator)
                 (lambda () (setq asserted (1+ asserted)))))
        (proof-electric-terminator)
        (proof-electric-terminator)
        (should (= asserted 1))
        (should (equal (buffer-string) "."))))))

(ert-deftest coq-double-hit-processes ()
  "A quick double hit processes up to the dot, inserted once."
  (with-double-hit-file
    (proof-electric-terminator-toggle 0)
    (proof-double-hit-toggle 1)
    (should proof-double-hit-enable)
    (type-dot)
    (type-dot)
    (wait-for-coq)
    (should (string-suffix-p "Definition y := 1." (buffer-string)))
    (should (= (proof-unprocessed-begin) (point-max)))))

(ert-deftest coq-double-hit-slow ()
  "Two slow hits insert two dots, without processing."
  (with-double-hit-file
    (proof-electric-terminator-toggle 0)
    (proof-double-hit-toggle 1)
    (type-dot)
    (sleep-for 0.6)
    (type-dot)
    (wait-for-coq)
    (should (string-suffix-p "Definition y := 1.." (buffer-string)))
    (should (= (proof-unprocessed-begin) (point-min)))))

(ert-deftest coq-double-hit-off ()
  "Without double hit nor electric terminator, \".\" is inserted."
  (with-double-hit-file
    (proof-electric-terminator-toggle 0)
    (proof-double-hit-toggle 0)
    (type-dot)
    (type-dot)
    (wait-for-coq)
    (should (string-suffix-p "Definition y := 1.." (buffer-string)))
    (should (= (proof-unprocessed-begin) (point-min)))))

(ert-deftest coq-double-hit-old-names ()
  "The old Coq names of the options still work."
  (with-double-hit-file
    (coq-double-hit-toggle 1)
    (should coq-double-hit-enable)
    (coq-double-hit-toggle 0)
    (should-not proof-double-hit-enable)))

(ert-deftest coq-double-hit-common-command ()
  "The terminator is bound in `coq-mode-map', common to all backends.
Proof General sets the backend function, and binding another key in
`coq-mode-map' to `coq-terminator-insert' works: a double hit on it
gives \".\" and processes up to it, a single hit inserts it."
  (with-double-hit-file
    (should (eq (key-binding ".") #'coq-terminator-insert))
    (should (eq (lookup-key coq-mode-map ".") #'coq-terminator-insert))
    (should (eq coq-terminator-insert-function #'proof-electric-terminator))
    (unwind-protect
        (progn
          (define-key coq-mode-map (kbd ";") #'coq-terminator-insert)
          (should (eq (key-binding ";") #'coq-terminator-insert))
          (proof-electric-terminator-toggle 0)
          (proof-double-hit-toggle 1)
          ;; a single hit inserts the key
          (type-key ?\;)
          (should (string-suffix-p "Definition y := 1;" (buffer-string)))
          (sleep-for 0.6)
          (delete-char -1)
          ;; a double hit inserts the terminator and processes
          (type-key ?\;)
          (type-key ?\;)
          (wait-for-coq)
          (should (string-suffix-p "Definition y := 1." (buffer-string)))
          (should (= (proof-unprocessed-begin) (point-max))))
      (define-key coq-mode-map (kbd ";") nil))))

(ert-deftest coq-double-hit-excludes-electric-terminator ()
  "The double hit and the electric terminator exclude each other."
  (with-double-hit-file
    ;; `proof-set-value' runs the actions of options only interactively
    (let ((noninteractive nil))
      (proof-electric-terminator-toggle 1)
      (proof-double-hit-toggle 1)
      (should proof-double-hit-enable)
      (should-not proof-electric-terminator-enable)
      (proof-electric-terminator-toggle 1)
      (should proof-electric-terminator-enable)
      (should-not proof-double-hit-enable)
      (proof-electric-terminator-toggle 0))))

(provide 'coq-test-double-hit)

;;; coq-test-double-hit.el ends here
