;;; coq-test-hook-variables.el --- Test variables shared with proof shell hooks  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Some hooks of proof-shell.el communicate with the hook functions
;; through dynamically bound variables: `queueitems' for
;; `proof-shell-extend-queue-hook', `string', `action' and
;; `scriptspan' for `proof-shell-insert-hook' and `prover-was-busy'
;; for `proof-shell-signal-interrupt-hook'.  Since proof-shell.el uses
;; lexical binding, these variables need special care, and interpreted
;; and byte-compiled code may behave differently.  Therefore, test each
;; hook with proof-shell.el loaded from source and byte-compiled.
;;
;; These tests do not need Coq, they only use the generic part of
;; Proof General.

;;; Code:

(require 'ert)
;; Make byte-compile-dest-file-function special before binding it below.
(require 'bytecomp)
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'proof-shell)

(defvar queueitems)
(defvar string)
(defvar action)
(defvar scriptspan)
(defvar prover-was-busy)


;;; utility functions

(defun hook-vars-load-proof-shell (mode)
  "Load proof-shell.el from source if MODE is `interpreted'.
Otherwise byte-compile it into a temporary file and load that."
  (let ((source (locate-library "proof-shell.el" t)))
    (if (eq mode 'interpreted)
        (load source nil t t)
      (let* ((elc (make-temp-file "proof-shell" nil ".elc"))
             (byte-compile-dest-file-function (lambda (_) elc)))
        (unwind-protect
            (progn
              (should (byte-compile-file source))
              (load elc nil t t))
          (delete-file elc))))))

(defmacro hook-vars-in-both-modes (&rest body)
  "Run BODY once with interpreted and once with byte-compiled proof-shell.el."
  (declare (indent 0))
  `(dolist (mode '(interpreted compiled))
     (hook-vars-load-proof-shell mode)
     (ert-info ((format "proof-shell.el %s" mode))
       ,@body)))


;;; tests

(ert-deftest hook-vars-extend-queue ()
  "Functions on `proof-shell-extend-queue-hook' can modify `queueitems'."
  (hook-vars-in-both-modes
    (with-temp-buffer
      (let ((proof-locked-span (make-overlay 1 1))
            (proof-queue-span (make-overlay 1 1))
            (proof-shell-extend-queue-hook
             (list (lambda () (setq queueitems (cdr queueitems)))))
            added)
        (cl-letf (((symbol-function 'proof-add-to-queue)
                   (lambda (items _mode) (setq added items))))
          (proof-extend-queue 1 '(require-item other-item)))
        (should (equal added '(other-item)))))))

(ert-deftest hook-vars-shell-insert ()
  "Functions on `proof-shell-insert-hook' see `action' and `scriptspan'
and can modify `string'."
  (hook-vars-in-both-modes
    (with-temp-buffer
      (let ((proof-shell-buffer (current-buffer))
            (proof-marker (make-marker))
            (proof-shell-strip-crs-from-input nil)
            (proof-shell-insert-hook
             (list (lambda ()
                     (should (eq action 'test-action))
                     (should (eq scriptspan 'test-span))
                     (setq string (concat "Modified " string))))))
        (cl-letf (((symbol-function 'scomint-send-input) #'ignore))
          (proof-shell-insert "Command." 'test-action 'test-span))
        (should (equal (buffer-string) "Modified Command."))))))

(ert-deftest hook-vars-interrupt ()
  "Functions on `proof-shell-signal-interrupt-hook' can set `prover-was-busy'.
Otherwise `proof-interrupt-process' signals an error when the prover
itself is idle, as it happens during background compilation."
  (hook-vars-in-both-modes
    (let ((proof-shell-busy nil)
          (proof-shell-signal-interrupt-hook
           (list (lambda () (setq prover-was-busy t)))))
      (cl-letf (((symbol-function 'proof-shell-live-buffer) (lambda () t)))
        (proof-interrupt-process)))))

(provide 'coq-test-hook-variables)

;;; coq-test-hook-variables.el ends here
