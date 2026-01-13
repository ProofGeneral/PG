;;; runtest.el --- Test -*- lexical-binding: t; -*-
;;
;; This file is part of Proof General.
;;
;; © Copyright 2020  Hendrik Tews
;;
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Coq Compile Tests (cct) --
;; ert tests for parallel background compilation for Coq
;;
;; Test that the right files are recompiled when changes are made in
;; the dependency hierarchy and that unaffected ancestors are not
;; touched.
;;
;; The following graph shows the file dependencies in this test:
;;
;;           a
;;          / \
;;         b   c
;;        /   / \
;;       d   e   f
;;      / \  |  /
;;     |   \ | /
;;     |    \|/
;;     g     h
;;
;;
;;
;;
;;
;;
;;

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)

;;; Data and utility functions

(defconst b-ancestors '("./b.v" "./d.v" "./g.v" "./h.v")
  "Ancestors of b.v.")

(defconst c-ancestors '("./c.v" "./e.v" "./f.v")
  "Ancestors of c.v.")

(defconst all-ancestors (append b-ancestors c-ancestors)
  "All ancestors.")

(defconst all-compiled-ancestors
  (mapcar #'cct-library-vo-of-v-file all-ancestors)
  "All vo ancestors files.")
  

(defun check-main-buffer (vo-times new-sum recompiled-files
                                       other-locked-files)
  "Perform various checks in buffer a.v.
See `cct-generic-check-main-buffer'."
  (cct-generic-check-main-buffer
   "a.v"                                ; main-buf
   21                                   ; main-unlocked
   37                                   ; main-locked
   28                                   ; main-sum-line
   new-sum
   vo-times
   recompiled-files
   `((22 . ,b-ancestors) (23 . ,c-ancestors)) ; require-ancestors
   other-locked-files
   18                                   ; other-locked-line
   ))


;;; The test itself

(ert-deftest cct-change-recompile ()
  "Test successful recompilation after changes and deletion."
  (find-file "a.v")

  ;; 1. process original content - compile everything
  (message "\n1. process original content - compile everything\n")
  (cct-process-to-line 38)
  (cct-check-locked 37 'locked)
  (cct-locked-ancestors 22 b-ancestors)
  (cct-locked-ancestors 23 c-ancestors)

  (let ((vo-times (cct-record-change-times all-compiled-ancestors))
        other-locked-files)

    ;; 2. retract and process again - nothing should be compiled
    (message "\n2. retract and process again - nothing should be compiled\n")
    (cct-process-to-line 21)
    (check-main-buffer vo-times "35" nil nil)

    ;; 3. change c and process again - only c should be compiled
    (message "\n3. change c and process again - only c should be compiled\n")
    (find-file "c.v")
    (push "c.v" other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "4")
    (check-main-buffer vo-times "36" '("./c.vo") other-locked-files)

    ;; 4. change h and process again - everything except g should get compiled
    (message (concat "\n4. change h and process again - "
                         "everything except g should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (find-file "h.v")
    (push "h.v" other-locked-files)
    (cct-check-locked 21 'locked)
    (cct-replace-last-word 21 "10")
    (check-main-buffer
     vo-times "38" '("./b.vo" "./c.vo" "./d.vo" "./e.vo" "./f.vo" "./h.vo")
     other-locked-files)
    
    ;; 5. change d and f and process again -
    ;; only b, c, d and f should get recompiled
    (message (concat "\n5. change d and f and process again - "
                         "only b, c, d and f should get recompiled"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (find-file "d.v")
    (push "d.v" other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "7")
    (find-file "f.v")
    (push "f.v" other-locked-files)
    (cct-check-locked 23 'unlocked)
    (cct-replace-last-word 23 "10")

    (check-main-buffer
     vo-times "45" '("./b.vo" "./c.vo" "./d.vo" "./f.vo") other-locked-files)

    ;; 6. change d and b and process again - only d and b should get compiled
    (message (concat "\n6. change d and b and process again - "
                         "only d and b should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (find-file "b.v")
    (push "b.v" other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "7")
    (set-buffer "d.v")
    (cct-check-locked 23 'unlocked)
    (cct-replace-last-word 23 "13")    ; increase by 6

    (check-main-buffer vo-times "56" '("./b.vo" "./d.vo")
                           other-locked-files)
    
    ;; 7. delete b and process again - only b should get compiled
    (message (concat "\n7. delete b and process again - "
                         "only b should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (delete-file "b.vo")
    (set-buffer "a.v")
    (cct-process-to-line 21)
    (check-main-buffer vo-times "56" '("./b.vo") other-locked-files)
    
    ;; 8. delete h and process again - everything except g should get compiled
    (message (concat "\n8. delete h and process again - "
                         "everything except g should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (delete-file "h.vo")
    (set-buffer "a.v")
    (cct-process-to-line 21)
    (check-main-buffer
     vo-times "56" '("./b.vo" "./c.vo" "./d.vo" "./e.vo" "./f.vo" "./h.vo")
     other-locked-files)
    
    ;; 9. change d, delete g and process again - only b, d and g should
    ;; get compiled
    (message (concat "\n9. change d, delete g and process again - "
                         "only b, d and g should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (set-buffer "d.v")
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "20")     ; increase by 7
    (delete-file "g.vo")
    (set-buffer "a.v")
    (cct-process-to-line 21)
    (check-main-buffer vo-times "63" '("./b.vo" "./d.vo" "./g.vo")
                           other-locked-files)
    
    ;; 10. delete d, change g and process again - only b, d and g should
    ;; get compiled
    (message (concat "\n10. delete d, change g and process again - "
                         "only b, d and g should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (delete-file "d.vo")
    (find-file "g.v")
    (push "g.v" other-locked-files)
    (cct-check-locked 21 'locked)
    (cct-replace-last-word 21 "15")     ; increase by 8
    (set-buffer "a.v")
    (cct-process-to-line 21)
    (check-main-buffer vo-times "71" '("./b.vo" "./d.vo" "./g.vo")
                           other-locked-files)
  ))

(provide 'runtest)

;;; runtest.el ends here
