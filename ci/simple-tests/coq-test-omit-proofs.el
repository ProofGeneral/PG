;; This file is part of Proof General.
;; 
;; © Copyright 2021  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Test the omit proofs feature
;;
;; Test that with proof-omit-proofs-option
;; - the proof _is_ processed when using a prefix argument
;; - in this case the proof has normal locked color
;; - without prefix arg, the proof is omitted
;; - the proof has omitted color then
;; - stuff before the proof still has normal color

;; Load stuff for `coq--version<'
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)

(defconst coq--post-v809 (coq--post-v809)
  "t if Coq is more recent than 8.8")

(message "omit tests run with Coq version %s; post-v809: %s"
         (coq-version t) coq--post-v809)

;; reimplement seq-some from the seq package
;; seq-some not present in emacs 24
;; XXX consider to switch to seq-some when support for emacs 24 is dropped
(defun list-some (pred list)
  "Return non-nil if PRED is satisfied for at least one element of LIST.
If so, return the first non-nil value returned by PRED."
  (let (res)
    (while (and (consp list) (not res))
      (setq res (funcall pred (car list)))
      (setq list (cdr list)))
    res))

(defun wait-for-coq ()
  "Wait until processing is complete."
  (while (or proof-second-action-list-active
             (consp proof-action-list))
    ;; (message "wait for coq/compilation with %d items queued\n"
    ;;          (length proof-action-list))
    ;;
    ;; accept-process-output without timeout returns rather quickly,
    ;; apparently most times without process output or any other event
    ;; to process.
    (accept-process-output nil 0.1)))
  
(defun reset-coq ()
  "Reset Coq and Proof General.
Do `proof-shell-exit' to kill Coq and reset the locked region and
a lot of other internal state of Proof General. Used at the
beginning of the test when several tests work on the same Coq
source file."
  (when (and (boundp 'proof-shell-buffer)
             (buffer-live-p proof-shell-buffer))
    (proof-shell-exit t)
    (message "Coq and Proof General reseted")))

(defun overlay-less (a b)
  "Compare two overlays.
Return t if overlay A has smaller size than overlay B and should
therefore have a higher priority."
  (let ((sa (- (overlay-end a) (overlay-start a)))
        (sb (- (overlay-end b) (overlay-start b))))
    (<= sa sb)))

(defun overlays-at-point-sorted ()
  "Return overlays at point in decreasing order of priority.
Works only if no overlay has a priority property. Same
'(overlays-at (point) t)', except that it also works on Emacs <= 25."
  (sort (overlays-at (point) t) 'overlay-less))

(defun first-overlay-face ()
  "Return the face of the first overlay/span that has a face property.
Properties configured in that face are in effect. Properties not
configured there may be taken from faces with less priority."
  (list-some
   (lambda (ov) (overlay-get ov 'face))
   ;; Need to sort overlays oneself, because emacs 25 returns overlays
   ;; in increasing instead of decreasing priority.
   (overlays-at-point-sorted)))

(ert-deftest omit-proofs-omit-and-not-omit ()
  "Test the omit proofs feature.
In particular, test that with proof-omit-proofs-option configured:
- the proof _is_ processed when using a prefix argument
- in this case the proof as normal locked color
- without prefix arg, the proof is omitted
- the proof has omitted color then
- stuff before the proof still has normal color "
  (setq proof-omit-proofs-option t
        proof-three-window-enable nil)
  (reset-coq)
  (find-file "omit_test.v")
  (goto-char (point-min))

  ;; Check 1: check that the proof is valid and omit can be disabled
  (message "1: check that the proof is valid and omit can be disabled")
  (should (search-forward "automatic test marker 4" nil t))
  (forward-line -1)
  ;; simulate C-u prefix argument
  (proof-goto-point '(4))
  (wait-for-coq)
  ;; Look into the *coq* buffer to find out whether the proof was
  ;; processed fully without error. This is necessary for Coq >= 8.11.
  ;; Coq < 8.11 prints a defined message, which ends up in *response*,
  ;; for those versions it would be better to simply check the
  ;; *response* buffer. However, consolidating all this is not easy,
  ;; therefore I check the message in *coq* also for Coq < 8.11.
  (with-current-buffer "*coq*"
    ;; output *coq* content for debugging
    ;; (message
    ;;  "*coq* content:\n%s"
    ;;  ;; (max 0 (- (point-max) 400))
    ;;  (buffer-substring-no-properties (point-min) (point-max)))

    (goto-char (point-max))
    ;; goto second last prompt
    (should (search-backward "</prompt>" nil t 2))
    ;; move behind prompt
    (forward-char 9)
    ;; There should be a Qed with no error or message after it
    (should
     (or
      ;; for Coq 8.11 and later
      (looking-at "Qed\\.\n\n<prompt>Coq <")
      ;; for Coq 8.10 and earlier
      ;; in 8.9 the message is on 1 line, in 8.10 on 3
      (looking-at "Qed\\.\n<infomsg>\n?classic_excluded_middle is defined"))))

  ;; Check 2: check proof-locked-face is active at marker 2 and 3
  (message "2: check proof-locked-face is active at marker 2 and 3")
  (should (search-backward "automatic test marker 2" nil t))
  (should (eq (first-overlay-face) 'proof-locked-face))
  (should (search-forward "automatic test marker 3" nil t))
  (should (eq (first-overlay-face) 'proof-locked-face))

  ;; Check 3: check that the second proof is omitted
  (message "3: check that the second proof is omitted")
  ;; first retract
  (should (search-backward "automatic test marker 1" nil t))
  (proof-goto-point)
  (wait-for-coq)
  ;; move forward again
  (should (search-forward "automatic test marker 4" nil t))
  (forward-line -1)
  (proof-goto-point)
  (wait-for-coq)
  (with-current-buffer "*response*"
    (goto-char (point-min))
    ;; There should be a declared message.
    (should (looking-at "classic_excluded_middle is declared")))

  ;; Check 4: check proof-omitted-proof-face is active at marker 3
  (message "4: check proof-omitted-proof-face is active at marker 3")
  (should (search-backward "automatic test marker 3" nil t))
  ;; debug overlay order
  ;; (mapc
  ;;  (lambda (ov)
  ;;    (message "OV %d-%d face %s"
  ;;             (overlay-start ov) (overlay-end ov) (overlay-get ov 'face)))
  ;;    (overlays-at-point-sorted))
  (should (eq (first-overlay-face) 'proof-omitted-proof-face))

  ;; Check 5: check proof-locked-face is active at marker 1 and 2
  (message "5: check proof-locked-face is active at marker 1 and 2")
  (should (search-backward "automatic test marker 1" nil t))
  (should (eq (first-overlay-face) 'proof-locked-face))
  (should (search-forward "automatic test marker 2" nil t))
  (should (eq (first-overlay-face) 'proof-locked-face)))

(ert-deftest omit-proofs-never-omit-hints ()
  "Test that proofs containing Hint are never omitted.
This test only checks that the face in the middle of the proof is
the normal `proof-locked-face'.

The sources for the test contain a local attribute in form of
'#[local]', which has been introduced only in Coq version 8.9."
  (skip-unless coq--post-v809)
  (setq proof-omit-proofs-option t
        proof-three-window-enable nil)
  (reset-coq)
  (find-file "omit_test.v")
  (goto-char (point-min))
  ;; Check that proofs with Hint commands are never omitted
  (message "Check that proofs with Hint commands are never omitted")
  (should (search-forward "automatic test marker 6" nil t))
  (forward-line -1)
  (proof-goto-point)
  (wait-for-coq)
  (should (search-backward "automatic test marker 5" nil t))
  (should (eq (first-overlay-face) 'proof-locked-face)))


(ert-deftest omit-proofs-never-omit-lets ()
  "Test for Let and proof omission.
Test that proofs for Let local declarations are never omitted and
that proofs of theorems following a Let definition are omitted.

This test only checks the faces in the middle of the proof."
  (setq proof-omit-proofs-option t
        proof-three-window-enable nil)
  (reset-coq)
  (find-file "omit_test.v")
  (goto-char (point-min))
  ;; Check that proofs for Let local declarations are never omitted.
  (message "Check that proofs for Let local declarations are never omitted.")
  (should (search-forward "automatic test marker 8" nil t))
  (forward-line -1)
  (proof-goto-point)
  (wait-for-coq)
  (should (search-backward "automatic test marker 7-1" nil t))
  (should (eq (first-overlay-face) 'proof-locked-face))

  ;; Check that theorems behind Let definitions are omitted.
  (message "Check that theorems behind Let definitions are omitted.")
  (should (search-forward "automatic test marker 7-2" nil t))
  (should (eq (first-overlay-face) 'proof-omitted-proof-face)))
