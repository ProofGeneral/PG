;;; coq-test-system.el --- Test coq-system.el  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the cache of `coq-detect-rocq-cli'.  Coq is not started.

;;; Code:

(require 'proof-site)
(proof-ready-for-assistant 'coq)

(require 'ert)
(require 'cl-lib)
(require 'coq-system)

(defvar coq--rocq-cli-cache)

(declare-function coq-detect-rocq-cli "coq-system")


(ert-deftest coq-test-system-rocq-cli-cache ()
  "`coq-detect-rocq-cli' runs a command once per command and `exec-path'."
  (let ((coq--rocq-cli-cache nil)
        (calls nil))
    (cl-letf (((symbol-function 'process-file)
               (lambda (program &rest _)
                 (push program calls)
                 (cond
                  ((equal program "fake-rocq")
                   (princ "Supported subcommands: c, top, dep")
                   1)
                  ((equal program "fake-coqtop") 0)
                  (t (error "No such program: %s" program))))))
      (should (coq-detect-rocq-cli "fake-rocq"))
      (should (coq-detect-rocq-cli "fake-rocq"))
      (should (equal calls '("fake-rocq")))
      (should-not (coq-detect-rocq-cli "fake-coqtop"))
      (should-not (coq-detect-rocq-cli "fake-coqtop"))
      (should (equal calls '("fake-coqtop" "fake-rocq")))
      ;; another exec-path, for example another opam switch
      (let ((exec-path (cons "/another/switch/bin" exec-path)))
        (should (coq-detect-rocq-cli "fake-rocq")))
      (should (equal calls '("fake-rocq" "fake-coqtop" "fake-rocq")))
      ;; a command that cannot be run is not cached
      (should-not (coq-detect-rocq-cli "missing"))
      (should-not (coq-detect-rocq-cli "missing"))
      (should (equal (cl-count "missing" calls :test #'equal) 2)))))

(provide 'coq-test-system)

;;; coq-test-system.el ends here
