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
;; This file contains common definitions for the automated tests of
;; parallel background compilation.


(defmacro cct-implies (p q)
  "Short-circuit logical implication.
Evaluate Q only if P is non-nil."
  `(or (not ,p) ,q))

(defun cct-goto-line (line)
  "Put point on start of line LINE.
Very similar to `goto-line', but the documentation of `goto-line'
says, programs should use this piece of code."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun cct-process-to-line (line)
  "Assert/retract to line LINE and wait until processing completed."
  (cct-goto-line line)
  (proof-goto-point)

  (while (or proof-second-action-list-active (consp proof-action-list))
    ;; (message "wait for coq/compilation with %d items queued\n"
    ;;          (length proof-action-list))
    ;;
    ;; accept-process-output without timeout returns rather quickly,
    ;; apparently most times without process output or any other event
    ;; to process.
    (accept-process-output nil 0.1)))

(defun cct-get-vanilla-span (line)
  "Get THE vanilla span for line LINE, report an error if there is none.
PG uses a number of overlapping and non-overlapping spans (read
overlays) in the asserted and queue region of the proof buffer,
see the comments in generic/proof-script.el. Spans of type
vanilla (stored at 'type in the span property list) are created
for real commands (not for comments). They hold various
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
  (save-excursion
    (set-buffer "*Messages*")
    (goto-char (point-max))
    (forward-line -1)
    (buffer-substring (point) (- (point-max) 1))))

(defun cct-check-locked (line locked-state)
  "Check that line LINE has locked state LOCKED-STATE
LOCKED-STATE must be 'locked or 'unlocked. This function checks
whether line LINE is inside or outside the asserted (locked)
region of the buffer and signals a test failure if not."
  (let ((locked (eq locked-state 'locked)))
    ;; (message "tcl line %d check %s: %s %s\n"
    ;;          line (if locked "locked" "unlocked")
    ;;          proof-locked-span
    ;;          (if (overlayp proof-locked-span)
    ;;              (span-end proof-locked-span)
    ;;            "no-span"))
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

(defun cct-locked-ancestors (line ancestors)
  "Check that the vanilla span at line LINE has ANCESTORS recorded.
The comparison treats ANCESTORS as set but the file names must
be `equal' as strings.

Ancestors are recoreded in the 'coq-locked-ancestors property of
the vanilla spans of require commands, see the in-file
documentation of coq/coq-par-compile.el."
  (let ((locked-ancestors
         (span-property (cct-get-vanilla-span line) 'coq-locked-ancestors)))
    (should
     (seq-set-equal-p locked-ancestors ancestors))))

(defun cct-file-newer (file time)
  "Check that FILE exists and its modification time is more recent than TIME."
  (let ((file-time (nth 5 (file-attributes file))))
    (should (and file-time (time-less-p time file-time)))))

(defun cct-configure-proof-general ()
  "Configure Proof General for test execution."
  (setq delete-old-versions t
        coq-compile-before-require t
        coq-compile-keep-going t
        proof-auto-action-when-deactivating-scripting 'retract
        proof-three-window-enable nil
        coq-compile-auto-save 'save-coq
        coq--debug-auto-compilation nil))
