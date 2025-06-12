;;; coq-test-three-window.el --- Test starting Proof General in three-pane mode
;; This file is part of Proof General.
;; 
;; © Copyright 2024  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Starting Proof General in three-pane mode
;; (`proof-three-window-enable' is t and
;; `proof-three-window-mode-policy' is 'smart) used to fail with
;; "window ... too small for splitting" for frame heights less then
;; three times `window-min-height' (which defaults to 4).  The problem
;; was relevant for Emacs 26.3, 27.1, and 27.2 running in batch mode
;; in docker containers, because they set their frame height to 9 (and
;; their width to 10) in such environments.  For this reason most Proof
;; General CI tests disable three pane mode in one or the other way.
;;
;; This file tests that the internal function `proof-select-three-b'
;; creates 3 windows if the frame height is big enough.  Additionally,
;; it is tested that one user command
;; (`proof-toggle-active-scripting') that used to be affected by the
;; bug does not signal an error, regardless of the frame size.  Both
;; functions are tested with three different frame sizes: too small
;; for 3 windows, just big enough for 3 windows, and the default frame
;; size.

;;; Code:

(require 'pg-response)

(defun reset-coq ()
  "Reset Coq and Proof General.
Do `proof-shell-exit' to kill Coq and reset the locked region and
a lot of other internal state of Proof General.  Used at the
beginning of the test when several tests work on the same Coq
source file."
  (when (and (boundp 'proof-shell-buffer)
             (buffer-live-p proof-shell-buffer))
    (proof-shell-exit t)
    (message "Coq and Proof General reseted")))


(defun test-proof-select-three-b-for-height (height expect-error)
  "Test `proof-select-three-b' in 3-pane mode for HEIGHT.
EXPECT-ERROR must be non-nil precisely if the frame height is
expected to be too small for 3 windows.  In this case nothing is
done here, because `proof-select-three-b' must not be called in
such situations.  Otherwise the function should not signal an
error and set up 3 windows."
  (if expect-error
      (message "Skip test-proof-select-three-b for height %d" height)
    (let ((b1 (get-buffer-create "test-b1"))
          (b2 (get-buffer-create "test-b2"))
          (b3 (get-buffer-create "test-b3"))
          ;; the following is set to its default value
          (proof-three-window-enable t))
      (reset-coq)
      (message "Check test-proof-select-three-b for height %d" height)
      (set-frame-height (selected-frame) height)
      (proof-select-three-b b1 b2 b3 'smart)
      (message "Apparently no error signaled in proof-select-three-b")
      (message "Check that there are 3 windows")
      (should (eq (length (window-list)) 3))

      ;; clean up
      (kill-buffer b1)
      (kill-buffer b2)
      (kill-buffer b3))))


(ert-deftest test-proof-select-three-b-too-small ()
  "Test `proof-select-three-b' for a frame height too small for 3 windows."
  (test-proof-select-three-b-for-height (- (* 3 window-min-height) 1) t))
          
(ert-deftest test-proof-select-three-b-just-big-enough ()
  "Test `proof-select-three-b' for a frame height just big enough for 3 windows."
  (test-proof-select-three-b-for-height (* 3 window-min-height) nil))
          
(ert-deftest test-proof-select-three-b-default-height ()
  "Test `proof-select-three-b' for the default frame height.
Note that for Emacs 26.3, 27.1, and 27.2, the default frame
height is 9 when running in a docker container."
  (test-proof-select-three-b-for-height
   (frame-height)
   (if (and
        ;; >= 26.3
        (or (> emacs-major-version 26)
            (and (eq emacs-major-version 26) (>= emacs-minor-version 3)))
        ;; < 28
        (< emacs-major-version 28))
       t
     nil)))


(defun test-activate-scripting-for-height (height num-win)
  "Test `proof-toggle-active-scripting' in 3-pane mode for HEIGHT.
The function should never signal an error and afterwards there
should be 3 windows if the frame has enough space and 2
otherwise.  Argument NUM-WIN specifies the expected number of
windows for HEIGHT."
  (let ((proof-three-window-enable t)
        (proof-three-window-mode-policy 'smart))
    (reset-coq)
    (message "Check proof-toggle-active-scripting for height %d" height)
    (set-frame-height (selected-frame) height)
    (find-file "some_file.v")
    (proof-toggle-active-scripting)
    (message "Apparently no error signaled in proof-toggle-active-scripting")
    (message "Check that there are %d windows" num-win)
    (should (eq (length (window-list)) num-win))))


(ert-deftest test-proof-toggle-active-scripting-too-small ()
  "Test `proof-toggle-active-scripting' in a frame too small for 3 windows."
  (test-activate-scripting-for-height (* 4 window-min-height) 2))

(ert-deftest test-proof-toggle-active-scripting-just-big-enough ()
  "Test `proof-toggle-active-scripting' in a frame just enough for 3 windows."
    (test-activate-scripting-for-height (+ (* 4 window-min-height) 1) 3))

(ert-deftest test-proof-toggle-active-scripting-default-height ()
  "Test `proof-toggle-active-scripting' with the default frame size.
Note that for Emacs 26.3, 27.1, and 27.2, the default frame
height is 9 when running in a docker container."
  (test-activate-scripting-for-height
   (frame-height)
   (if (and
        ;; >= 26.3
        (or (> emacs-major-version 26)
            (and (eq emacs-major-version 26) (>= emacs-minor-version 3)))
        ;; < 28
        (< emacs-major-version 28))
       2
     3)))

(provide 'coq-test-three-window)

;;; coq-test-three-window.el ends here
