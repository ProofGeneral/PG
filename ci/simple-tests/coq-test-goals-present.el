;; This file is part of Proof General.
;; 
;; © Copyright 2021  Hendrik Tews
;; 
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;; 
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test that Proof General shows goals correctly in various situations.

;; Some tests show different behaviour in 8.7, load stuff for `coq--version<'
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)

(defconst coq--post-v87 (not (coq--version< (coq-version t) "8.8"))
  "t if Coq is more recent than 8.7")

(message "goal present tests run with Coq version %s; post-v87: %s"
         (coq-version t) coq--post-v87)


;;; Coq source code for tests 

(defconst coq-src-proof
  "
Lemma a : 1 + 1 = 2.
Proof using.
"
  "Coq source code for checking goals after ``Proof''.")

(defconst coq-src-comment
  "
Lemma a : 1 + 1 = 2.
Proof using.
  simpl.
  (* some comment *)
"
  "Coq source code for checking goals after a comment.")

(defconst coq-src-auto
  "
Lemma a : 1 + 1 = 3.
Proof using.
  auto.
"
  "Coq source code for checking goals after ``auto''.")

(defconst coq-src-simpl
  "
Lemma a : 1 + 1 = 2.
Proof using.
  simpl.
"
  "Coq source code for checking goals after a ``simpl''.")

(defconst coq-src-error
  "
Lemma a : 1 + 1 = 3.
Proof using.
  simpl.
  X.
"
  "Coq source code for checking goals after an error.")

(defconst coq-src-admitted
  "Lemma a : forall(P27X : Prop), P27X.
Proof using.
  intros P27X.
  Admitted.
"
  "Coq source for checking that the goals buffer is reset after Admitted.")

(defconst coq-src-update-goal-after-error
  "
(* code taken from pull request #429 *)

(* set printing width explicitely, otherwise it will be rediculously small. *)
Set Printing Width 80.

Definition eq_one (i : nat) := i = 1.
(* eq_one is delta_reducible but it should not be reduced. *)

Lemma foo: (eq_one 1 -> False) -> False.
(* point A: first process to this point *)
Proof.
  intros H. 
  intro.
  (* point B: Then process the two intros - the second one
     triggers an error. The goals should be updated to show the
     state after the first intro.
  *)
"
  "Coq source code for checking that goals are updated even in case of error.")


;;; utility functions

(defun record-buffer-content (buf)
  "Record buffer content of BUF via `message' for debugging.
BUF should be a string."
  (with-current-buffer buf
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (message "%s buffer contains %d chars: %s" buf (length content) content))))

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


;;; define the tests

(defun goals-after-test (coq-src msg)
  "Test that Proof General shows goals after processing COQ-SRC.
Process COQ-SRC in a new buffer in one step and check that the
goals buffer is not empty afterwards."
  (message "goals-after-test: Check goals are present after %s." msg)
  (setq proof-three-window-enable nil)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src)
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*goals*")

          ;; check that there is a goal in the goals buffer
          (with-current-buffer "*goals*"
            (goto-char (point-min))
            (should (looking-at "1 \\(sub\\)?goal (ID"))))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest goals-after-proof ()
  "Test goals are present after ``Proof''."
  :expected-result (if coq--post-v87 :failed :passed)
  (goals-after-test coq-src-proof "Proof"))

(ert-deftest goals-after-comment ()
  "Test goals are present after a comment."
  :expected-result :failed
  (goals-after-test coq-src-comment "comment"))

(ert-deftest goals-after-auto ()
  "Test goals are present after ``auto''."
  :expected-result  (if coq--post-v87 :failed :passed)
  (goals-after-test coq-src-auto "auto"))

(ert-deftest goals-after-simpl ()
  "Test goals are present after ``simpl''."
  (goals-after-test coq-src-simpl "simpl"))

(ert-deftest goals-after-error ()
  "Test goals are present after an error."
  :expected-result :failed
  (goals-after-test coq-src-error "error"))

(ert-deftest goals-reset-after-admitted ()
  :expected-result :failed
  "The goals buffer is reset after an Admitted."
  (message
   "goals-reset-after-admitted: Check that goals are reset after Admitted.")
  (setq proof-three-window-enable nil)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src-admitted)

          ;; Need to assert the Admitted alone, therefore first assert
          ;; until before the Admitted.
          (goto-char (point-min))
          (should (re-search-forward "intros P27X" nil t))
          (forward-line 1)
          (proof-goto-point)
          (wait-for-coq)

          (forward-line 1)
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*goals*")

          ;; check that the old goal is not present in the goals buffer
          (with-current-buffer "*goals*"
            (goto-char (point-min))
            (should (not (re-search-forward "P27X" nil t)))))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest update-goals-after-error ()
  "Test goals are updated after an error."
  :expected-result :failed
  (message "update-goals-after-error: Check goals are updated after error")
  (setq proof-three-window-enable nil)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src-update-goal-after-error)
          (goto-char (point-min))
          (should (re-search-forward "point A" nil t))
          (beginning-of-line)
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*goals*")

          ;; the complete goal should be present
          (with-current-buffer "*goals*"
            (goto-char (point-min))
            (should (re-search-forward "(eq_one 1 -> False) -> False" nil t)))

          (should (re-search-forward "point B" nil t))
          (beginning-of-line)
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*goals*")

          ;; the hypothesis H should be present
          (with-current-buffer "*goals*"
            (goto-char (point-min))
            (should (re-search-forward "H : eq_one 1 -> False" nil t))))
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))
