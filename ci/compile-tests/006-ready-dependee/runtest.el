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
;; Test recompilation and ancestor registration for the case a
;; dependency is in state ready already.
;;
;; The following graph shows the file dependencies in this test:
;;
;;           a
;;          / \
;;         b   c
;;         |   |____________
;;         d                \
;;         |                 |
;;         e--f--g--h--i--j--k
;;
;; The idea is that the coqdep chain from b to j takes so long, that k
;; has been compiled and is in state ready before the coqdep results
;; from j are processed.  This works - unless the disk cache is cold.
;; If it works the test triggers two bugs.  First, k is locked with the
;; require command of c. Second, the modification time of k is not
;; propagated to j, such that the whole chain from j to b is not
;; recompiled after k has changed.

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)

;;; Data and utility functions

(defconst b-ancestors '("./b.v" "./d.v" "./e.v" "./f.v" "./g.v"
                        "./h.v" "./i.v" "./j.v" "./k.v")
  "Ancestors of b.v.")

(defconst c-ancestors '("./c.v")
  "Ancestors of c.v.")

(defconst all-ancestors (append b-ancestors c-ancestors)
  "All ancestors.")

(defconst all-compiled-ancestors
  (mapcar #'cct-library-vo-of-v-file all-ancestors)
  "All vo ancestors files.")

(defun cct-replace-last-word (line word)
  "Replace last word in line LINE with WORD.
In current buffer, go to the end of line LINE and one word
backward.  Replace the word there with WORD."
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
  "Test successful recompilation for a dependency in state ready."
  ;;(setq coq--debug-auto-compilation t)
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
    (cct-check-main-buffer vo-times "9" nil nil)

    ;; 3. change k and process again - everything should be compiled
    (message "\n3. change k and process again - everything should be compiled\n")
    (find-file "k.v")
    (push "k.v" other-locked-files)
    (cct-check-locked 21 'locked)
    (cct-replace-last-word 21 "5")
    (cct-check-main-buffer
     vo-times "10"
     '("./b.vo" "./c.vo" "./d.vo" "./e.vo" "./f.vo" "./g.vo"
       "./h.vo" "./i.vo" "./j.vo" "./k.vo")
     other-locked-files)
  ))

(provide 'runtest)

;;; runtest.el ends here
