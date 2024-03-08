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
  "Lemma a : forall(P : Prop), P.
Proof using.
  intros P.
  Admitted.
"
  "Coq source for checking that the goals buffer is reset after Admitted.")

(defconst coq-src-no-more-goals
  "
Lemma a : 1 + 1 = 2.
Proof using.
  simpl.
  auto.
"
  "Coq source code for checking that the goals buffer is reset when
no goals are left.")

(defconst coq-src-qed
  "
Lemma a : 1 + 1 = 2.
Proof using.
  simpl.
  auto.
Qed.
"
  "Coq source code for checking that the goals buffer is reset after Qed.")

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

(defun goals-buffer-should-be-empty (pos msg)
  "Check that `*goals*' is empty after asserting/retracting to POS.
MSG is only used in a message, it should tell after which action
the goals buffer is expected to be empty."
  (message "Check that goals buffer is empty after %s" msg)
  (goto-char pos)
  (proof-goto-point)
  (wait-for-coq)
  ;; (record-buffer-content "*coq*")
  ;; (record-buffer-content "*goals*")

  ;; check that the goals buffer is empty
  (with-current-buffer "*goals*"
    (should (equal (point-min) (point-max)))))

(defun goals-buffer-should-get-reset (coq-src coq-stm msg)
  "Check that the goals buffer is reset.
Put the string COQ-SRC into a buffer and assert until the first
occurrence of COQ-STM, which should be a regular expression. At
this point the goals buffer needs to contain something. Then
assert to the end of COQ-SRC and check that the goals buffer has
been reset. MSG is used in messages only. It shouls say after
which action the goals buffer should have been reset."
  (message "Check that goals are reset after %s." msg)
  (setq proof-three-window-enable nil)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src)

          ;; First fill the goals buffer by asserting until the line
          ;; after the first occurrence of COQ-STM.

          (goto-char (point-min))
          (should (re-search-forward coq-stm nil t))
          (forward-line 1)
          (message "*goals* should be non-empty after asserting until after %s"
                   coq-stm)
          (proof-goto-point)
          (wait-for-coq)
          ;; there should be something in the goals buffer now
          (with-current-buffer "*goals*"
            (should (not (equal (point-min) (point-max)))))

          (goals-buffer-should-be-empty (point-max) msg))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))
  

(ert-deftest goals-after-proof ()
  "Test goals are present after ``Proof''."
  (goals-after-test coq-src-proof "Proof"))

(ert-deftest goals-after-comment ()
  "Test goals are present after a comment."
  (goals-after-test coq-src-comment "comment"))

(ert-deftest goals-after-auto ()
  "Test goals are present after ``auto''."
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
  (goals-buffer-should-get-reset coq-src-admitted "intros P" "Admitted"))

(ert-deftest goals-reset-no-more-goals ()
  "The goals buffer is reset when there are no more goals."
  (goals-buffer-should-get-reset coq-src-no-more-goals
                                 "Proof using" "no more goals"))

(ert-deftest goals-reset-qed ()
  :expected-result :failed
  "The goals buffer is reset after Qed."
  (goals-buffer-should-get-reset coq-src-qed
                                 "Proof using" "Qed"))

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
