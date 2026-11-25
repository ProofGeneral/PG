;;; coq-test-error-environment.el --- Test the folding of the environments of errors  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the folding of the hypotheses before the error messages
;; (`coq-fold-error-environments'), without Coq.  The test with Coq is
;; in coq-test-output-windows.el: the folded error fits its window.

;;; Code:

(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'ert)
(require 'coq-goals)

;; let-bound in the tests: dynamic
(defvar coq-fold-error-environment)
(defvar coq-response-buffer-function)

(defconst error-environment-text
  "In environment
n, m : nat
f : nat -> nat
H' :
  forall k : nat,
  f k = k
x := n + m : nat
The term \"n\" has type \"nat\" while it is expected to have type \"True\".
"
  "An error message with an environment of 5 hypotheses.")

(defmacro error-environment-test (text &rest body)
  "Run BODY in a buffer with TEXT, folded environments by default."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (let ((coq-fold-error-environment t)
           (coq-response-buffer-function #'current-buffer))
       ,@body)))

(defun error-environment-overlays ()
  "The overlays of the environments in the current buffer, in order."
  (sort (seq-filter (lambda (ov) (overlay-get ov 'coq-error-environment))
                    (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(ert-deftest error-environment-fold ()
  "The header and the hypotheses are replaced by their number."
  (error-environment-test error-environment-text
    (coq-fold-error-environments)
    (let ((ovs (error-environment-overlays)))
      (should (= (length ovs) 1))
      (should (= (overlay-start (car ovs)) (point-min)))
      (should (equal (substring-no-properties
                      (overlay-get (car ovs) 'display))
                     "▸ In environment (5 hypotheses)"))
      ;; the error message stays visible
      (should (string-prefix-p "\nThe term"
                               (buffer-substring (overlay-end (car ovs))
                                                 (point-max)))))
    ;; once only
    (coq-fold-error-environments)
    (should (= (length (error-environment-overlays)) 1))))

(ert-deftest error-environment-end-of-line ()
  "An environment announced at the end of a line, until a blank line."
  (error-environment-test "Cannot infer this placeholder of type \"C nat\" \
\(no type class instance
found) in environment:
n : nat

Other message
"
    (coq-fold-error-environments)
    (let ((ovs (error-environment-overlays)))
      (should (= (length ovs) 1))
      (should (equal (buffer-substring (overlay-start (car ovs))
                                       (overlay-end (car ovs)))
                     "in environment:\nn : nat"))
      (should (equal (substring-no-properties
                      (overlay-get (car ovs) 'display))
                     "▸ in environment (1 hypothesis)")))))

(ert-deftest error-environment-several ()
  "Each environment is folded, not the texts without hypotheses."
  (error-environment-test (concat error-environment-text
                                  "In environment\nThe end.\n"
                                  "Mentioning In environment in a line.\n"
                                  error-environment-text)
    (coq-fold-error-environments)
    (should (= (length (error-environment-overlays)) 2))))

(ert-deftest error-environment-unfolded ()
  "Unfolded, the header is a button with an arrow."
  (error-environment-test error-environment-text
    (let ((coq-fold-error-environment nil))
      (coq-fold-error-environments)
      (let ((ov (car (error-environment-overlays))))
        (should (equal (buffer-substring (overlay-start ov) (overlay-end ov))
                       "In environment"))
        (should (equal (overlay-get ov 'before-string) "▾ "))
        (should-not (overlay-get ov 'display))))))

(ert-deftest error-environment-toggle ()
  "RET on the header unfolds the environments, and folds them again."
  (error-environment-test error-environment-text
    (coq-fold-error-environments)
    (goto-char (point-min))
    (should (eq (key-binding (kbd "RET")) #'coq-toggle-error-environment))
    (coq-toggle-error-environment)
    (should-not coq-fold-error-environment)
    (should-not (overlay-get (car (error-environment-overlays)) 'display))
    (coq-toggle-error-environment)
    (should coq-fold-error-environment)
    (should (overlay-get (car (error-environment-overlays)) 'display))))

(provide 'coq-test-error-environment)

;;; coq-test-error-environment.el ends here
