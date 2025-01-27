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


(defconst coq-src-update-goal-after-search
  "
Lemma g : 1 + 1 = 2.
Proof using.
  simpl.
  Search (0 + _).
"
  "Coq source code for checking that goals are up-to-date after Search.")


(defconst coq-src-update-goal-after-check
  "
Lemma h : 1 + 2 = 3.
Proof using.
  simpl.
  Check plus_O_n.
"
  "Coq source code for checking that goals are up-to-date after Check.")


;;; utility functions

(defun record-buffer-content (buf)
  "Record buffer content of BUF via `message' for debugging.
BUF should be a buffer as string or buffer object."
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

(defun goals-after-test (coq-src msg check-response-nonempty)
  "Test that Proof General shows goals after processing COQ-SRC.
Process COQ-SRC in a new buffer in one step and check that the
goals buffer is not empty afterwards. If CHECK-RESPONSE-NONEMPTY
is non-nil, additionally check that the response buffer is
non-empty, i.e., shows some message, and is visible in some
window also in two-pane mode."
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
          (with-current-buffer proof-goals-buffer
            (goto-char (point-min))
            (should (looking-at "1 \\(sub\\)?goal (ID")))

          (when check-response-nonempty
            (message
             "goals-after-test: Check response buffer is nonempty after %s."
             msg)
            (with-current-buffer proof-response-buffer
              (should (not (equal (point-min) (point-max)))))
            (message
             "goals-after-test: Check response buffer is visible after %s."
             msg)
            (should (get-buffer-window proof-response-buffer))))

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
  (with-current-buffer proof-goals-buffer
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
          (with-current-buffer proof-goals-buffer
            (should (not (equal (point-min) (point-max)))))

          (goals-buffer-should-be-empty (point-max) msg))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))
  

(ert-deftest goals-after-proof ()
  "Test goals are present after ``Proof''."
  :expected-result (if coq--post-v87 :failed :passed)
  (goals-after-test coq-src-proof "Proof" nil))

(ert-deftest goals-after-comment ()
  "Test goals are present after a comment."
  :expected-result :failed
  (goals-after-test coq-src-comment "comment" nil))

(ert-deftest goals-after-auto ()
  "Test goals are present after ``auto''."
  :expected-result  (if coq--post-v87 :failed :passed)
  (goals-after-test coq-src-auto "auto" nil))

(ert-deftest goals-after-simpl ()
  "Test goals are present after ``simpl''."
  (goals-after-test coq-src-simpl "simpl" nil))

(ert-deftest goals-after-error ()
  "Test goals are present after an error."
  :expected-result :failed
  (goals-after-test coq-src-error "error" t))

(ert-deftest goals-reset-after-admitted ()
  "The goals buffer is reset after an Admitted."
  (goals-buffer-should-get-reset coq-src-admitted "intros P" "Admitted"))

(ert-deftest goals-reset-no-more-goals ()
  "The goals buffer is reset when there are no more goals."
  (goals-buffer-should-get-reset coq-src-no-more-goals
                                 "Lemma a" "no more goals"))

(ert-deftest goals-reset-qed ()
  :expected-result :failed
  "The goals buffer is reset after Qed."
  (goals-buffer-should-get-reset coq-src-qed
                                 "Proof using" "Qed"))

(defun update-goals-when-response (coq-src first-pos goal-2nd msg)
  "Test goals are up-to-date after an error or a command that produces response.
Process COQ-SRC up to the line after the first match of regular
expression FIRST-POS. At this point the goals buffer should not
be empty. Process now COQ-SRC up to the end. If GOAL-2ND is a
regular expression as a string, then check that the goals have
been updated to contain a match for GOAL-2ND. If GOAL-2ND is no
string, only check that the goals buffer is non-empty. In any
case, check that the response buffer is not empty and visible in
two-pane mode."
  
  (message "update-goals-when-response: Check goals are updated after %s" msg)
  (setq proof-three-window-enable nil)
  (let (buffer)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src)
          (goto-char (point-min))
          (should (re-search-forward first-pos nil t))
          (forward-line 1)
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*goals*")

          ;; goals should be present
          (message "Check that goals are present")
          (with-current-buffer proof-goals-buffer
            (should (not (equal (point-min) (point-max)))))

          (goto-char (point-max))
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*goals*")

          (with-current-buffer proof-goals-buffer
            (goto-char (point-min))
            (if (stringp goal-2nd)
                (progn
                  (message "Check that goals have been updated")
                  (should (re-search-forward goal-2nd nil t)))
              (message "Check that goals are still present")
              (should (not (equal (point-min) (point-max))))))

          ;; something should be in the response buffer
          (message "Check that there is some response present")
          (with-current-buffer proof-response-buffer
            (should (not (equal (point-min) (point-max)))))

          (message "Check that the response is visible in two-pane mode")
          (should (get-buffer-window proof-response-buffer)))

      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest goals-up-to-date-at-error ()
  "Check that goals are updated before showing the error."
  :expected-result :failed
  (update-goals-when-response coq-src-update-goal-after-error
                              "Lemma foo"
                              "H : eq_one 1 -> False"
                              "error"))

(ert-deftest goals-up-to-date-after-search-one-step ()
  "Check that goals are still present before showing result of one search cmd.
This test checks a single Search command inside a proof. After
processing that Search command alone, the goals buffer should not
be empty and the response buffer should contain something and be
visible in two-pane mode."
  (update-goals-when-response coq-src-update-goal-after-search
                              "simpl"
                              t
                              "Search"))

(ert-deftest goals-updated-after-search-many-steps ()
  "Check that goals are updated before showing result of search cmd.
This test checks several commands inside a proof with a final
Search command. After processing these commands, the goals buffer
should have been updated and the response buffer should contain
something and be visible in two-pane mode."
  :expected-result :failed
  (update-goals-when-response coq-src-update-goal-after-search
                              "Lemma g"
                              "2 = 2"
                              "Search"))

(ert-deftest goals-up-to-date-after-check-one-step ()
  "Check that goals are still present before showing result of one check cmd.
This test checks a single Check command inside a proof. After
processing that Check command alone, the goals buffer should not
be empty and the response buffer should contain something and be
visible in two-pane mode."
  (update-goals-when-response coq-src-update-goal-after-check
                              "simpl"
                              t
                              "Check"))

(ert-deftest goals-updated-after-check-many-steps ()
  "Check that goals are updated before showing result of check cmd.
This test checks several commands inside a proof with a final
Check command. After processing these commands, the goals buffer
should have been updated and the response buffer should contain
something and be visible in two-pane mode."
  :expected-result :failed
  (update-goals-when-response coq-src-update-goal-after-check
                              "Lemma h"
                              "3 = 3"
                              "Check"))
