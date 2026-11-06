;;; coq-test-file-variables.el --- Test the file variables of Coq  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the priority of the file variables (and directory variables)
;; over the project file (_CoqProject, _RocqProject) for
;; `coq-load-path' and `coq-prog-args', and the choice of the Coq
;; program by `coq-pg-detect-programs', run just before starting Coq:
;; detected by default, kept when customized or asked.  Coq is not
;; started.

;;; Code:

(require 'proof-site)
(proof-ready-for-assistant 'coq)

(require 'ert)

(defvar coq-load-path)
(defvar coq-prog-args)
(defvar coq-prog-name)
(defvar coq-compiler)
(defvar coq-dependency-analyzer)
(defvar proof-prog-name)
(defvar proof-prog-name-ask)

(declare-function coq-pg-detect-programs "coq")
(declare-function coq-autodetect-progname "coq-mode")


;;; utility functions

(defmacro coq-test-file-variables-with-file (project local-variables &rest body)
  "Run BODY in a buffer visiting a Coq file, with PROJECT and LOCAL-VARIABLES.
PROJECT is the content of a _RocqProject file next to it, or nil for
none.  LOCAL-VARIABLES is a list of strings \"VAR: VALUE\" for the
Local Variables section of the file.  The variable `dir' is bound to
the directory of the file in BODY."
  (declare (indent 2) (debug t))
  `(let* ((dir (file-name-as-directory (make-temp-file "file_variables" t)))
          (file (expand-file-name "a.v" dir))
          (enable-local-variables :all)
          (local-variables ,local-variables)
          buffer)
     (unwind-protect
         (progn
           (when ,project
             (with-temp-file (expand-file-name "_RocqProject" dir)
               (insert ,project)))
           (with-temp-file file
             (insert "Definition x := 0.\n")
             (when local-variables
               (insert "(* Local Variables: *)\n")
               (dolist (var local-variables)
                 (insert "(* " var " *)\n"))
               (insert "(* End: *)\n")))
           (find-file file)
           (setq buffer (current-buffer))
           ,@body)
       (when buffer
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-directory dir t))))

(defmacro coq-test-file-variables-detect (&rest body)
  "Run `coq-pg-detect-programs' then BODY, restoring the global options."
  (declare (indent 0) (debug t))
  `(let ((proof-prog-name proof-prog-name)
         (proof-prog-name-ask proof-prog-name-ask)
         (coq-compiler coq-compiler)
         (coq-dependency-analyzer coq-dependency-analyzer)
         (coq-prog-name coq-prog-name))
     ,@body))

(defconst coq-test-file-variables-project
  "-R theories Foo\n-arg -w -arg all\n"
  "Project file of the tests.")


;;; tests

(ert-deftest coq-test-file-variables-project-file ()
  "Without file variables, the project file sets the load path and the args."
  (coq-test-file-variables-with-file coq-test-file-variables-project nil
    (should (equal coq-load-path
                   `((rec ,(expand-file-name "theories" dir) "Foo"))))
    (should (equal coq-prog-args '("-w" "all")))))

(ert-deftest coq-test-file-variables-over-project-file ()
  "File variables have priority over the project file, one by one."
  (coq-test-file-variables-with-file coq-test-file-variables-project
      '("coq-load-path: ((\"lib\" \"Bar\"))")
    (should (equal coq-load-path '(("lib" "Bar"))))
    (should (equal coq-prog-args '("-w" "all"))))
  (coq-test-file-variables-with-file coq-test-file-variables-project
      '("coq-prog-args: (\"-type-in-type\")")
    (should (equal coq-load-path
                   `((rec ,(expand-file-name "theories" dir) "Foo"))))
    (should (equal coq-prog-args '("-type-in-type"))))
  (coq-test-file-variables-with-file coq-test-file-variables-project
      '("coq-load-path: ((\"lib\" \"Bar\"))"
        "coq-prog-args: (\"-type-in-type\")")
    (should (equal coq-load-path '(("lib" "Bar"))))
    (should (equal coq-prog-args '("-type-in-type")))))

(ert-deftest coq-test-file-variables-dir-locals ()
  "Directory variables also have priority over the project file."
  (coq-test-file-variables-with-file coq-test-file-variables-project nil
    (with-temp-file (expand-file-name ".dir-locals.el" dir)
      (insert "((coq-mode . ((coq-prog-args . (\"-type-in-type\")))))\n"))
    (revert-buffer t t)
    (should (equal coq-prog-args '("-type-in-type")))
    (should (equal coq-load-path
                   `((rec ,(expand-file-name "theories" dir) "Foo"))))))

(ert-deftest coq-test-file-variables-prog-name-detected ()
  "By default, the Coq program is detected just before starting Coq."
  (coq-test-file-variables-with-file nil nil
    (coq-test-file-variables-detect
      (setq proof-prog-name "not-a-coq-program")
      (coq-pg-detect-programs)
      (should (equal proof-prog-name (coq-autodetect-progname)))
      (should (equal coq-prog-name proof-prog-name)))))

(ert-deftest coq-test-file-variables-prog-name-asked ()
  "The Coq program given by the user with `proof-prog-name-ask' is kept."
  (coq-test-file-variables-with-file nil nil
    (coq-test-file-variables-detect
      (setq proof-prog-name-ask t)
      (setq proof-prog-name "/asked/rocq")
      (coq-pg-detect-programs)
      (should (equal proof-prog-name "/asked/rocq"))
      (should (equal coq-prog-name "/asked/rocq")))))

(ert-deftest coq-test-file-variables-prog-name-customized ()
  "A customized Coq program is kept."
  (coq-test-file-variables-with-file nil nil
    (coq-test-file-variables-detect
      (let ((customized (get 'coq-prog-name 'customized-value)))
        (unwind-protect
            (progn
              (put 'coq-prog-name 'customized-value '("/customized/rocq"))
              (setq proof-prog-name "/customized/rocq")
              (coq-pg-detect-programs)
              (should (equal proof-prog-name "/customized/rocq")))
          (put 'coq-prog-name 'customized-value customized))))))

(provide 'coq-test-file-variables)

;;; coq-test-file-variables.el ends here
