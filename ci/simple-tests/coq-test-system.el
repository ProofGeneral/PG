;;; coq-test-system.el --- Test coq-system.el  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the cache of `coq-detect-rocq-cli' and the round trip between
;; the options of a project file and `coq-load-path'.  Coq is not
;; started.

;;; Code:

(require 'proof-site)
(proof-ready-for-assistant 'coq)

(require 'ert)
(require 'cl-lib)
(require 'coq-system)

(defvar coq--rocq-cli-cache)
(defvar coq-load-path-include-current)

(declare-function coq-detect-rocq-cli "coq-system")
(declare-function coq--read-options-from-project-file "coq-project")
(declare-function coq--extract-load-path "coq-project")
(declare-function coq-include-options "coq-project")


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

(defun coq-test-system-roundtrip (project pre-v85)
  "Return non-nil if the include options of PROJECT survive a round trip.
Read as `coq-load-path' then printed back, with PRE-V85 compatibility."
  (with-no-warnings                     ; obsolete variable
    (let* ((parsed (coq--read-options-from-project-file project))
           (coq-load-path-include-current nil)
           (extracted (coq--extract-load-path parsed nil)))
      (equal (apply #'append parsed)
             (coq-include-options extracted nil pre-v85)))))

(ert-deftest coq-test-system-options-roundtrip ()
  "The include options of a project file are printed back unchanged."
  (dolist (project '("-Q /test Test" "-Q /test \"\"" "-R /test Test" "-I /test"))
    (should (coq-test-system-roundtrip project nil)))
  ;; Coq < 8.5 has no -Q
  (dolist (project '("-R /test Test" "-I /test"))
    (should (coq-test-system-roundtrip project t))))

(provide 'coq-test-system)

;;; coq-test-system.el ends here
