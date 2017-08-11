;; coq-autotest.el: tests of Coq Proof General (in progress).

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel
;; Portions © Copyright 2012, Hendrik Tews
;; Portions © Copyright 2017, Clément Pit-Claudel
;; Portions © Copyright 2016-2017, Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

;; You can run these by issuing "make test.coq" in PG home dir.

(eval-when-compile
  (require 'cl-lib))

(require 'pg-autotest)

(require 'proof-site)
(defvar coq-compile-before-require)

(unless (bound-and-true-p byte-compile-current-file)

  (pg-autotest start 'debug)

  ;; new multiple file handling for Coq gives interactive queries
  ;; continually unless we set this.
  (setq proof-auto-action-when-deactivating-scripting 'retract)
  (setq proof-no-fully-processed-buffer t)

  (pg-autotest log ".autotest.log")  ; convention

  (pg-autotest timestart 'total)

  (pg-autotest remark "Testing standard examples...")
  (pg-autotest script-wholefile "coq/example.v")
  (pg-autotest script-wholefile "coq/example-tokens.v")
  (pg-autotest script-wholefile "coq/ex-module.v")
  (proof-shell-wait)

  (pg-autotest remark "Regression testing bug cases...")
  (pg-autotest script-wholefile "etc/coq/parsingcheck-410.v")
  ;(pg-autotest script-wholefile "etc/coq/parsingcheck-412.v")

  (pg-autotest remark "Testing prove-as-you-go (not replay)")
  (find-file ".autotest.v")
  (erase-buffer) ; just in case exists
  (setq buffer-file-name nil)
  (pg-autotest eval (proof-electric-terminator-toggle 1))
  (pg-autotest eval (insert "Module test")) ; no \n
  (proof-electric-terminator)
  ;; Note: with new multiple-file code, above fires up process.
  ;; If we do (proof-shell-wait) here, we get a deadlock.
  (pg-autotest eval (insert " Goal forall (A B:Prop),(A /\\ B) -> (B /\\ A)"))
  (proof-electric-terminator)
  (proof-shell-wait)
  (pg-autotest eval (insert "\ntauto."))
  (backward-char)
  (proof-electric-terminator) ; shouldn't insert another or delete present
  (proof-shell-wait)
;; FIXME: next test fails, why?
  (pg-autotest assert-full)
  ;; TODO: test switching active scripting buffer after
  ;; an error (see cvs log for proof-script.el 10.68)
;; FIXME: bug, this causes "region is read only"
;;  (pg-autotest eval (insert " End test"))
;;  (proof-electric-terminator)
  (set-buffer-modified-p nil)
  (kill-buffer ".autotest.v")
  (proof-shell-wait)

  ;; TODO: test indentation.  E.g., to avoid regression of Trac #416
  
  (pg-autotest script-wholefile "etc/coq/multiple-plain/a.v")
  (pg-autotest script-wholefile "etc/coq/multiple-plain/b.v")
  (pg-autotest script-wholefile "etc/coq/multiple-plain/c.v")
  (pg-autotest script-wholefile "etc/coq/multiple-plain/a.v")

  (pg-autotest remark "Testing multiple-file recompilation...")
  (setq coq-compile-before-require t)
  (pg-autotest script-wholefile "coq/ex/test-cases/multiple-files-single-dir/f.v")
  (proof-shell-wait)
  (find-file "d.v")
  (set-buffer-modified-p t)
  (save-buffer)
  (pg-autotest-test-retract-file  "coq/ex/test-cases/multiple-files-single-dir/f.v")
  (proof-shell-wait)
  (pg-autotest script-wholefile "coq/ex/test-cases/multiple-files-single-dir/f.v")

  (pg-autotest remark "Complete.")

  (pg-autotest timetaken 'total)

  (pg-autotest exit))
