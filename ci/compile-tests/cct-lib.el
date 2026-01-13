;;; cct-lib.el --- Background compilation test helpers
;; This file is part of Proof General.
;; 
;; © Copyright 2020 - 2021  Hendrik Tews
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
;; This file contains common definitions for the automated tests of
;; parallel background compilation.

;;; Code:

;; Require some libraries for the elisp compilation.  When compiling,
;; nothing is loaded, but all Proof General directories are in the
;; load path.  When this file is loaded as part of a test, proof-site
;; has been loaded but only the generic subdir is in the load path.
;; This file references variables from coq-compile-common and
;; functions from proof-shell, therefore coq-compile-common must be
;; required for compilation.  When running a test, these files would be
;; loaded anyway when the test visits the first Coq file.  For
;; executing tests, the Coq subdir will only be added to the load path
;; when the first Coq file is visited.  Therefore we have to add it
;; here via proof-ready-for-assistant.  For compilation, we have to
;; require proof-site, otherwise proof-ready-for-assistant won't be
;; defined.
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-compile-common)
(require 'ert)


(defvar cct--debug-tests nil
  "Set to t to get more output during test runs.")

(defvar cct-before-busy-waiting-hook nil
  "Hooks run by ‘cct-process-to-line’ before busy waiting.")

(defvar cct-after-busy-waiting-hook nil
  "Hooks run by ‘cct-process-to-line’ after busy waiting.")


(defmacro cct-implies (p q)
  "Short-circuit logical implication.
Evaluate Q only if P is non-nil."
  `(or (not ,p) ,q))

(defun cct-list-subset (l1 l2)
  "Return t if all elements of L1 are in L2 (compared by `equal')."
  (let ((res t))
    (while (and l1 res)
      (unless (member (pop l1) l2)
        (setq res nil)))
    res))

;; Reimplementation of seq-set-equal-p for lists, because
;; seq-set-equal-p is not present before emacs 26.
;; XXX consider seq-set-equal-p when emacs 25 support is dropped.
(defun cct-list-set-equal (l1 l2)
  "Return t if L1 and L2 contain the elements (compared by `equal')."
  (and (cct-list-subset l1 l2)
       (cct-list-subset l2 l1)))

(defun cct-goto-line (line)
  "Put point on start of line LINE.
Very similar to `goto-line', but the documentation of `goto-line'
says, programs should use this piece of code."
  (goto-char (point-min))
  (forward-line (1- line))
  (cl-assert (eq (line-number-at-pos) line) nil
             "point not at required line in cct-goto-line"))

(defun cct-library-vo-of-v-file (v-src-file)
  "Return .vo file name for V-SRC-FILE.
Changes the suffix from .v to .vo.  V-SRC-FILE must have a .v suffix."
  (concat v-src-file "o"))

(defun cct-record-change-time (file)
  "Return cons of FILE and its modification time.
The modification time is an Emacs time value, it's nil if file
cannot be accessed."
  (cons file (nth 5 (file-attributes file))))

(defun cct-record-change-times (files)
  "Return an assoc list of FILES with their modification times.
The modification time is an Emacs time value, it's nil if file
cannot be accessed."
  (mapcar 'cct-record-change-time files))

(defun cct-split-change-times (file-change-times files)
  "Split assoc list FILE-CHANGE-TIMES.
FILE-CHANGE-TIMES must be an assoc list and FILES must be a list
of some of the keys of FILE-CHANGE-TIMES.  This function returns
two associations lists (as cons cell).  The car contains those
associations in FILE-CHANGE-TIMES with keys not in FILES, the cdr
contains those with keys in FILES."
  (let (out in)
    (dolist (file-time file-change-times (cons out in))
      (if (member (car file-time) files)
          (push file-time in)
        (push file-time out)))))

(defun cct-replace-last-word (line word)
  "Replace last word in line LINE with WORD.
In current buffer, go to the end of line LINE and one word
backward.  Replace the word there with WORD."
  (cct-goto-line line)
  (end-of-line)
  (backward-word)
  (kill-word 1)
  (insert word))

(defun cct-process-to-line (line)
  "Assert/retract to start of line LINE and wait until processing completed.
Runs `cct-before-busy-waiting-hook' and
`cct-after-busy-waiting-hook' before and after busy waiting for
the prover.  In many tests these hooks are not used."
  (when cct--debug-tests
    (message "assert/retrect to line %d in buffer %s" line (buffer-name)))
  (cct-goto-line line)
  (proof-goto-point)

  (run-hooks 'cct-before-busy-waiting-hook)
  (while (or proof-second-action-list-active (consp proof-action-list))
    ;; (message "wait for coq/compilation with %d items queued\n"
    ;;          (length proof-action-list))
    ;;
    ;; accept-process-output without timeout returns rather quickly,
    ;; apparently most times without process output or any other event
    ;; to process.
    (accept-process-output nil 0.1))
  (run-hooks 'cct-after-busy-waiting-hook))

(defun cct-wait-for-second-stage ()
  "Wait until second stage compilation is complete.
Runs `cct-before-busy-waiting-hook' and
`cct-after-busy-waiting-hook' before and after busy waiting."
  (run-hooks 'cct-before-busy-waiting-hook)
  (when cct--debug-tests
    (message
     "%s start waiting for vok/vio2vo timer %s 2nd-stage-in-progress %s"
     (current-time-string)
     coq--par-second-stage-delay-timer
     coq--par-second-stage-in-progress))
  (while (or coq--par-second-stage-delay-timer
             coq--par-second-stage-in-progress)
    (accept-process-output nil 0.5)
    (when cct--debug-tests
      (message "%s wait for vok/vio2vo timer %s 2nd-stage-in-progress %s"
               (current-time-string)
               coq--par-second-stage-delay-timer
               coq--par-second-stage-in-progress)))
  (run-hooks 'cct-after-busy-waiting-hook))

(defun cct-get-vanilla-span (line)
  "Get THE vanilla span for line LINE, report an error if there is none.
PG uses a number of overlapping and non-overlapping spans (read
overlays) in the asserted and queue region of the proof buffer,
see the comments in generic/proof-script.el.  Spans of type
vanilla (stored at 'type in the span property list) are created
for real commands (not for comments).  They hold various
information that is used, among others, for backtracking.

This function returns the vanilla span that covers line LINE and
reports a test failure if there is none or more than one vanilla spans."
  (let (spans)
    (cct-goto-line line)
    (setq spans (spans-filter (overlays-at (point)) 'type 'vanilla))
    (should (eq (length spans) 1))
    (car spans)))

(defun cct-last-message-line ()
  "Extract the last line from the *Messages* buffer.
Useful if the message is not present in the echo area any more
and `current-message' does not return anything."
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (forward-line -1)
    (buffer-substring (point) (- (point-max) 1))))

(defun cct-check-locked (line locked-state)
  "Check that line LINE has locked state LOCKED-STATE.
LOCKED-STATE must be 'locked or 'unlocked.  This function checks
whether line LINE is inside or outside the asserted (locked)
region of the buffer and signals a test failure if not."
  (let ((locked (eq locked-state 'locked)))
    (when cct--debug-tests
      (message (concat "check lock state in buffer %s: line %d should be %s;\n"
                       "\tlocked-span: %s ends at char %s in line %s")
               (buffer-name)
               line (if locked "locked" "unlocked")
               proof-locked-span
               (if (overlayp proof-locked-span)
                   (span-end proof-locked-span)
                 "<no-span>")
               (if (overlayp proof-locked-span)
                   (line-number-at-pos (span-end proof-locked-span))
                 "<no-span>")))
    (cl-assert (or locked (eq locked-state 'unlocked))
               nil "test-check-locked 2nd argument wrong")
    (cct-goto-line line)
    (should (cct-implies locked (span-end proof-locked-span)))

    (should
     (or
      (and (not locked)
           (or (not proof-locked-span) (not (span-end proof-locked-span))))
      (and (span-end proof-locked-span)
           (funcall (if locked '< '>)
                    (point) (span-end proof-locked-span)))))))

(defun cct-check-files-locked (line lock-state files)
  "Check that all FILES at line number LINE have lock state LOCK-STATE.
LOCK-STATE must be either 'locked or 'unlocked.  FILES must be
list of file names."
  (when cct--debug-tests
    (message "check files %s at line %d: %s"
             (if (eq lock-state 'locked) "locked" "unlocked") line files))
  (save-current-buffer
    (mapc
     (lambda (file)
       (find-file file)
       (cct-check-locked line lock-state))
     files)))

(defun cct-locked-ancestors (line ancestors)
  "Check that the vanilla span at line LINE has ANCESTORS recorded.
The comparison treats ANCESTORS as set but the file names must
be `equal' as strings.

Ancestors are recorded in the 'coq-locked-ancestors property of
the vanilla spans of require commands, see the in-file
documentation of coq/coq-par-compile.el."
  (let ((locked-ancestors
         (span-property (cct-get-vanilla-span line) 'coq-locked-ancestors)))
    (should
     (cct-list-set-equal locked-ancestors ancestors))))

(defun cct-file-unchanged (file time)
  "Check that modification time of FILE equals TIME.
Used to check that FILE has not been changed since TIME was
recorded before."
  (let ((file-time (nth 5 (file-attributes file))))
    (when cct--debug-tests
      (message "file %s should be unchanged, recorded time: %s now: %s"
               file
               (format-time-string "%H:%M:%S.%3N" time)
               (format-time-string "%H:%M:%S.%3N" file-time)))
    (should
     (and file-time (equal time file-time)))))

(defun cct-unmodified-change-times (file-time-assoc)
  "Check that files in FILE-TIME-ASSOC have not been changed.
FILE-TIME-ASSOC must be an association list of files and Emacs
times as returned by `cct-record-change-times' or
`cct-split-change-times'.  This function checks that the
modification time of files in FILE-TIME-ASSOC equals the time
recorded in FILE-TIME-ASSOC, i.e., that the file has not been
changed since FILE-TIME-ASSOC has been recorded."
  (when cct--debug-tests
    (message "Files should be unchanged: %s"
             (mapconcat
              (lambda (file-time) (car file-time))
              file-time-assoc
              ", ")))
  (mapc
   (lambda (file-time-cons)
     (cct-file-unchanged (car file-time-cons) (cdr file-time-cons)))
   file-time-assoc))

(defun cct-file-newer (file time)
  "Check that FILE exists and its modification time is more recent than TIME."
  (let ((file-time (nth 5 (file-attributes file))))
    (when cct--debug-tests
      (message "file %s in %s should be changed, recorded time: %s now: %s"
               file
               default-directory
               (if time (format-time-string "%H:%M:%S.%3N" time) "---")
               (if file-time
                   (format-time-string "%H:%M:%S.%3N" file-time)
                 "---")))
    (should (and file-time (time-less-p time file-time)))))

(defun cct-older-change-times (file-time-assoc)
  "Check that files exist and have been changed.
FILE-TIME-ASSOC must be an association list of files and Emacs
times as returned by `cct-record-change-times' or
`cct-split-change-times'.  This function checks that the files in
FILE-TIME-ASSOC do exist and that their modification time is more
recent than in the association list, i.e., they have been updated
or changed since recording the time in the association."
  (when cct--debug-tests
    (message "Files should have been changed: %s"
             (mapconcat
              (lambda (file-time) (car file-time))
              file-time-assoc
              ", ")))
  (mapc
   (lambda (file-time-cons)
     (cct-file-newer (car file-time-cons) (cdr file-time-cons)))
   file-time-assoc))

(defun cct-files-are-readable (files)
  "Check that FILES exist and are readable."
  (when cct--debug-tests
    (message "Files should exist and be readable: %s" files))
  (mapc (lambda (fname) (should (file-readable-p fname))) files))

(defun cct-files-dont-exist (files)
  "Check that FILES don't exist."
  (when cct--debug-tests
    (message "Files should not exist: %s" files))
  (mapc (lambda (fname) (should-not (file-exists-p fname))) files))

(defun cct-generic-check-main-buffer
    (main-buf main-unlocked main-locked main-sum-line new-sum
              vo-times recompiled-files require-ancestors
              other-locked-files other-locked-line)
  "Perform various checks for recompilation in MAIN-BUF.
MAIN-BUF is a buffer, MAIN-LOCKED and MAIN-SUM-line are line
numbers in that buffer.  NEW-SUM is a number as string.  VO-TIMES
is an association list of files and Emacs times as returned by
`cct-record-change-times' or `cct-split-change-times'.
RECOMPILED-FILES is a list of some of the files in VO-TIMES.
REQUIRE-ANCESTORS is a list of cons cells, each cons containing a
line number in MAIN-BUF (which should contain a require) and a
list of ancestor files that should get registered in the span of
that require.  OTHER-LOCKED-FILES is a list of buffers and
OTHER-LOCKED-LINE is a common line number in those files.

This function combines all the following tests in this order:
- line MAIN-UNLOCKED in MAIN-BUF is unlocked
- after replacing the sum on line MAIN-SUM-LINE with NEW-SUM,
  MAIN-BUF can be processed until line MAIN-LOCKED
- files in VO-TIMES not listed in RECOMPILED-FILES have the same
  last change time as in VO-TIMES
- files in RECOMPILED-FILES have a newer change time
- the spans in lines in REQUIRE-ANCESTORS have precisely the
  ancestors registered as specified in REQUIRE-ANCESTORS.
- all the buffers in OTHER-LOCKED-FILES are locked until line
  OTHER-LOCKED-LINE."
  (let (splitted)
    (set-buffer main-buf)
    (cct-check-locked main-unlocked 'unlocked)
    (cct-replace-last-word main-sum-line new-sum)
    (cct-process-to-line (1+ main-locked))
    (cct-check-locked main-locked 'locked)
    (setq splitted (cct-split-change-times vo-times recompiled-files))
    ;; (message "check file dates, unmodified %s, modified %s"
    ;;          (car splitted) (cdr splitted))
    (cct-unmodified-change-times (car splitted))
    (cct-older-change-times (cdr splitted))
    (mapc
     (lambda (line-ancestors)
       (cct-locked-ancestors (car line-ancestors) (cdr line-ancestors)))
     require-ancestors)
    (mapc
     (lambda (b)
       (set-buffer b)
       (cct-check-locked other-locked-line 'locked))
     other-locked-files)))

(defun cct-configure-proof-general ()
  "Configure Proof General for test execution."
  (setq delete-old-versions t
        coq-compile-before-require t
        coq-compile-keep-going t
        proof-auto-action-when-deactivating-scripting 'retract
        proof-three-window-enable nil
        coq-compile-auto-save 'save-coq
        coq--debug-auto-compilation nil
        )
  (custom-set-variables '(coq-compile-quick 'ensure-vo))
  (when (< coq--internal-max-jobs 4)
    (custom-set-variables '(coq-max-background-compilation-jobs 4)))
  (message "config coq-max-background-compilation-jobs: %s (means %d)"
           coq-max-background-compilation-jobs
           coq--internal-max-jobs))

(defun configure-delayed-coq ()
  "Configure PG to honor artificial delays in background compilation.
Configure Proof General to use coqdep-delayed and coqc-delayed
from directory ../bin.  These scripts delay the start of the real
coqdep and coqc as specified in the Coq source file, see
../bin/compile-test-start-delayed.

This function uses relative file names and must be called in a
test subdirectory parallel to the bin directory."
  (let ((bin-dir (file-truename "../bin")))
    (add-to-list 'exec-path bin-dir)
    (setenv "PATH" (concat bin-dir ":" (getenv "PATH")))
    (custom-set-variables
     '(coq-dependency-analyzer "coqdep-delayed")
     '(coq-compiler "coqc-delayed"))))

(provide 'cct-lib)

;;; cct-lib.el ends here
