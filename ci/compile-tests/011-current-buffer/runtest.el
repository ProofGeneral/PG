;;; runtest.el --- Test -*- lexical-binding: t; -*-
;;
;; This file is part of Proof General.
;;
;; © Copyright 2024  Hendrik Tews
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
;; Test that parallel background compilation is not confused by local
;; variables in unrelated buffers.
;;
;; The dependencies in this test are:
;; 
;;           a     c    d
;;           |
;;           b
;;
;; Files c and d are completely independent of file a and file b and
;; not processed by Coq.  The idea is that files c or d come from a
;; different project that uses a different `coq-compiler' or
;; `coq-dependency-analyzer', see also PG issue #797. These different
;; local settings should not confuse the ongoing background
;; compilation of file b for processing file a in a script buffer.
;; File c sets `coq-compiler' as local variable and file d sets
;; `coq-dependency-analyzer' as local variable.

;;; Code:

;; require cct-lib for the elisp compilation, otherwise this is present already
(require 'cct-lib "ci/compile-tests/cct-lib")

;;; set configuration
(cct-configure-proof-general)
(configure-delayed-coq)

(defvar switch-buffer-while-waiting nil
  "Switch buffer in busy waiting hooks when t.
Whether the hook functions `switch-to-other-buffer-while-waiting'
and `switch-back-after-waiting' switch to some other buffer or
not is controlled by this variable.  If t, switch to the buffer in
`cdv-buffer' before starting busy waiting and switch back to the
buffer in `av-buffer' after busy waiting.")

(defvar av-buffer nil
  "Buffer to switch back after busy waiting.
See `switch-buffer-while-waiting'.")

(defvar cdv-buffer nil
  "Buffer to switch to before busy waiting.
See `switch-buffer-while-waiting'.")

(defun switch-to-other-buffer-while-waiting ()
  "Hook to switch current buffer before busy waiting.
Hook function for `cct-before-busy-waiting-hook'.  Switches to
`cdv-buffer' if `switch-buffer-while-waiting' is t."
  (when (and switch-buffer-while-waiting cdv-buffer)
    (message "Switch to buffer c.v while busy waiting")
    (set-buffer cdv-buffer)))

(defun switch-back-after-waiting ()
  "Hook to switch current buffer back after busy waiting.
Hook function for `cct-after-busy-waiting-hook'.  Switches back to
`av-buffer' if `switch-buffer-while-waiting' is t."
  (when (and switch-buffer-while-waiting av-buffer)
    (message "Switch back to buffer a.v after busy waiting")
    (set-buffer av-buffer)))

(add-hook 'cct-before-busy-waiting-hook #'switch-to-other-buffer-while-waiting)
(add-hook 'cct-after-busy-waiting-hook #'switch-back-after-waiting)


;;; The tests itself

(ert-deftest test-current-buffer-vok ()
  "Check that second stage compilation uses the right local variables.
Second stage compilation (vok and vio2vo) should use the local
variables from the original scripting buffer, see also PG issue
#797."
  (unwind-protect
      (progn
        (message "\nRun test-current-buffer-vok")

        ;; configure 2nd stage
        (if (coq--post-v811)
            (setq coq-compile-vos 'vos-and-vok)
          (setq coq-compile-quick 'quick-and-vio2vo))

        ;; (setq cct--debug-tests t)
        ;; (setq coq--debug-auto-compilation t)
        (find-file "a.v")
        (setq av-buffer (current-buffer))

        (find-file "c.v")
        (setq cdv-buffer (current-buffer))
        (set-buffer av-buffer)

        (message (concat "Settings in a.v:\n"
                         "  coqdep: %s\n  coqc: %s\n  PATH %s\n"
                         "  exec-path: %s\n  detected coq version: %s")
         coq-dependency-analyzer
         coq-compiler
         (getenv "PATH")
         exec-path
         coq-autodetected-version)

        ;; Work around existing .vos and .vok files from other tests in
        ;; this file.
        (message "\ntouch dependency b.v to force complete (re-)compilation")
        (should (set-file-times "b.v"))

        (message "\nProcess a.v to end, including compilation of dependency b.v")
        (cct-process-to-line 27)
        (cct-check-locked 26 'locked)

        (with-current-buffer cdv-buffer
          (message
           (concat "\nWait for 2nd stage (vok/vio2vo) compilation "
                   "in buffer b.v with"
                   "\n  coqdep: %s\n  coqc: %s")
           coq-dependency-analyzer
           coq-compiler))

        (setq switch-buffer-while-waiting t)

        ;; This will temporarily switch to buffer c.v, which sets
        ;; coq-compiler as local variable, see the hooks above
        (cct-wait-for-second-stage)

        (message "search for coq-error error message in compile-response buffer")
        (with-current-buffer coq--compile-response-buffer
          (goto-char (point-min))
          (should
           (not
            (re-search-forward "Error: coq-error has been executed" nil t)))))

    ;; clean up
    (dolist (buf (list av-buffer cdv-buffer))
      (when buf
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)))
    (setq switch-buffer-while-waiting nil)))

(ert-deftest test-current-buffer-coqdep ()
  "Check that dependency analysis uses the right local variables.
Dependency analysis during parallel background compilation (i.e.,
running `coqdep` on dependencies) should use the local variables
from the original scripting buffer, see also PG issue #797."
  (unwind-protect
      (progn
        (message "\nRun test-current-buffer-coqdep")

        ;; (setq cct--debug-tests t)
        ;; (setq coq--debug-auto-compilation t)
        (find-file "a.v")
        (setq av-buffer (current-buffer))

        (find-file "d.v")
        (setq cdv-buffer (current-buffer))
        (set-buffer av-buffer)

        (message (concat "Settings in a.v:\n"
                         "  coqdep: %s\n  coqc: %s\n  PATH %s\n"
                         "  exec-path: %s\n  detected coq version: %s")
         coq-dependency-analyzer
         coq-compiler
         (getenv "PATH")
         exec-path
         coq-autodetected-version)

        (with-current-buffer cdv-buffer
          (message
           (concat "\nProcess a.v to end while visiting d.v with"
                   "\n  coqdep: %s\n  coqc: %s")
           coq-dependency-analyzer
           coq-compiler))

        (setq switch-buffer-while-waiting t)

        ;; This will temporarily switch to buffer c.v, which sets
        ;; coq-compiler as local variable, see the hooks above.
        (cct-process-to-line 27)

        ;; (with-current-buffer coq--compile-response-buffer
        ;;   (message "coq-compile-response:\n%s\n<<<End"
        ;;            (buffer-substring-no-properties (point-min) (point-max))))

        (cct-check-locked 26 'locked))

    ;; clean up
    (dolist (buf (list av-buffer cdv-buffer))
      (when buf
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)))
    (setq switch-buffer-while-waiting nil)))

(ert-deftest test-current-buffer-coqc ()
  "Check that compilation of dependencies uses the right local variables.
Compilation of dependencies during parallel background
compilation (i.e., running `coqc` on dependencies) should use the
local variables from the original scripting buffer, see also PG
issue #797.

To ensure we only see errors from running `coqc`, we temporarily
switch to buffer c.v, which sets `coq-compiler' but leaves
`coq-dependency-analyzer' alone."
  (unwind-protect
      (progn
        (message "\nRun test-current-buffer-coqc")

        ;; (setq cct--debug-tests t)
        ;; (setq coq--debug-auto-compilation t)
        (find-file "a.v")
        (setq av-buffer (current-buffer))

        (find-file "c.v")
        (setq cdv-buffer (current-buffer))
        (set-buffer av-buffer)

        (message (concat "Settings in a.v:\n"
                         "  coqdep: %s\n  coqc: %s\n  PATH %s\n"
                         "  exec-path: %s\n  detected coq version: %s")
         coq-dependency-analyzer
         coq-compiler
         (getenv "PATH")
         exec-path
         coq-autodetected-version)

        (with-current-buffer cdv-buffer
          (message (concat "\nProcess a.v to end, "
                           "including compilation of dependency b.v\n"
                           "  while temporarily visiting c.v with\n"
                           "  coqdep: %s\n  coqc: %s")
           coq-dependency-analyzer
           coq-compiler))
        
        ;; Work around existing .vos and .vok files from other tests in
        ;; this file.
        (message "\ntouch dependency b.v to force complete (re-)compilation")
        (should (set-file-times "b.v"))

        (setq switch-buffer-while-waiting t)

        ;; This will temporarily switch to buffer c.v, which sets
        ;; coq-compiler as local variable, see the hooks above.
        (cct-process-to-line 27)

        ;; (with-current-buffer coq--compile-response-buffer
        ;;   (message "coq-compile-response:\n%s\n<<<End"
        ;;            (buffer-substring-no-properties (point-min) (point-max))))

        (cct-check-locked 26 'locked))

    ;; clean up
    (dolist (buf (list av-buffer cdv-buffer))
      (when buf
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)))
    (setq switch-buffer-while-waiting nil)))

(provide 'runtest)

;;; runtest.el ends here
