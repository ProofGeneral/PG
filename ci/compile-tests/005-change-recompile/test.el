;; This file is part of Proof General.
;;
;; © Copyright 2020  Hendrik Tews
;;
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

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
  (mapcar 'cct-library-vo-of-v-file all-ancestors)
  "All vo ancestors files.")
  

(defun cct-replace-last-word (line word)
  "Replace last word in line LINE with WORD.
In current buffer, go to the end of line LINE and one word
backward. Replace the word there with WORD."
  (cct-goto-line line)
  (end-of-line)
  (backward-word)
  (kill-word 1)
  (insert word))

(defun cct-check-main-buffer (vo-times new-sum recompiled-files
                                       other-locked-files)
  "Perform various checks for recompilation in buffer a.v.
Combine all the following tests in this order:
- line 21 in a.v is unlocked
- after replacing the sum on line 28 with NEW-SUM, a.v can be
  processed until line 38
- files in VO-TIMES not listed in RECOMPILED-FILES have the same
  last change time as in VO-TIMES
- files in RECOMPILED-FILES have a newer change time
- the spans in line 22 and 23 hold the ancestors of b and c, respectively.
- all the buffers in OTHER-LOCKED-FILES are locked until line 18."
  (let (splitted)
    (set-buffer "a.v")
    (cct-check-locked 21 'unlocked)
    (cct-replace-last-word 28 new-sum)
    (cct-process-to-line 38)
    (cct-check-locked 37 'locked)
    (setq splitted (cct-split-change-times vo-times recompiled-files))
    (cct-unmodified-change-times (car splitted))
    (cct-older-change-times (cdr splitted))
    (cct-locked-ancestors 22 b-ancestors)
    (cct-locked-ancestors 23 c-ancestors)
    (mapc
     (lambda (b)
       (set-buffer b)
       (cct-check-locked 18 'locked))
     other-locked-files)))


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
    (cct-check-main-buffer vo-times "35" nil nil)

    ;; 3. change c and process again - only c should be compiled
    (message "\n3. change c and process again - only c should be compiled\n")
    (find-file "c.v")
    (push "c.v" other-locked-files)
    (cct-check-locked 23 'locked)
    (cct-replace-last-word 23 "4")
    (cct-check-main-buffer vo-times "36" '("./c.vo") other-locked-files)

    ;; 4. change h and process again - everything except g should get compiled
    (message (concat "\n4. change h and process again - "
                         "everything except g should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (find-file "h.v")
    (push "h.v" other-locked-files)
    (cct-check-locked 21 'locked)
    (cct-replace-last-word 21 "10")
    (cct-check-main-buffer
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

    (cct-check-main-buffer
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

    (cct-check-main-buffer vo-times "56" '("./b.vo" "./d.vo")
                           other-locked-files)
    
    ;; 7. delete b and process again - only b should get compiled
    (message (concat "\n7. delete b and process again - "
                         "only b should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (delete-file "b.vo")
    (set-buffer "a.v")
    (cct-process-to-line 21)
    (cct-check-main-buffer vo-times "56" '("./b.vo") other-locked-files)
    
    ;; 8. delete h and process again - everything except g should get compiled
    (message (concat "\n8. delete h and process again - "
                         "everything except g should get compiled\n"))
    (setq vo-times (cct-record-change-times all-compiled-ancestors))
    (delete-file "h.vo")
    (set-buffer "a.v")
    (cct-process-to-line 21)
    (cct-check-main-buffer
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
    (cct-check-main-buffer vo-times "63" '("./b.vo" "./d.vo" "./g.vo")
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
    (cct-check-main-buffer vo-times "71" '("./b.vo" "./d.vo" "./g.vo")
                           other-locked-files)
  ))
