;;; coq-test-goals-present.el --- Test that Proof General shows goals correctly in various situations
;;
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
;; Test that Proof General shows goals correctly in various
;; situations.  Test also that in other situations the response buffer
;; contains the right output and is visible in two-pane mode.

;; global configuration for this file
;; all tests in this file shall run in two-pane mode
;;; Code:

(setq proof-three-window-enable nil)
;; Some Emacs versions run with a frame width of 10 in batch mode
;; inside the container. The line breaks make some regular expressions
;; fail, therfore disable printing width adapting.
(setq coq-auto-adapt-printing-width nil)


;; Some tests show different behaviour in 8.7, load stuff for `coq--version<'
(require 'proof-site)
(proof-ready-for-assistant 'coq)
(require 'coq-system)

(defconst coq--between-v814-v815 (and (coq--post-v814) (coq--pre-v816))
  "Non-nil if Coq is either 8.14 or 8.15.")

(defconst coq--post-8-20 (not (coq--version< (coq-version t) "9.0alpha"))
  "Non-nil if Coq is 9.0 or higher.")


(message (concat "goal/response present tests run with Coq version %s; \n\t"
                 "between 8.14-8.15 %s; post-8.20 %s")
         (coq-version t) coq--between-v814-v815 coq--post-8-20)


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

(defconst coq-src-report-response-check
  "Definition x := 1.

Lemma y : forall(a b : nat), S (a + b) = a + S b.
Proof using.
  intros a b.
  apply plus_n_Sm.
Qed.
"
  "Coq source code for response buffer visibility tests.
Used in `check-response-present' for all `response-buffer-visible-*' tests.")

(defconst coq-src-not-declared-section-variable
  "Section A.
  Variable P : Prop.
  Hypothesis p_true : P.

  Lemma a : P.
  Proof using.
    trivial.
    (* marker A *)
  Qed.
"
  "Coq source for ert-deftest's error-message-visible-at-qed-*.")

(defconst coq-src-queuemode-for-show-require
  (if coq--post-8-20
      "Require Export Lists.ListDef.\n"
    "Require Export Coq.Lists.List.\n")
  "Require command to use lists.
Starting in 9.0 the standard library containing Coq.Lists.List is in a
separate opam package, which might not be installed in the testing
container.  There use only stuff from the prelude, which is contained in
package rocq-core.")

(defconst coq-src-queuemode-for-show-remainder
  "Open Scope list_scope.

Inductive tree : Type :=
  Subtrees : list tree -> tree.

Fixpoint list_create(n : nat)(t : tree) : list tree :=
  match n with
  | 0 => nil
  | S n => t :: (list_create n t)
  end.

Fixpoint build_tree(n m : nat) : tree :=
  match n with
  | 0 => Subtrees nil
  | S n => Subtrees (list_create m (build_tree n m))
  end.

Lemma a :
  build_tree 6 6 = Subtrees nil.
Proof using.    (* marker A *)
  cbv.
  trivial.
"
  "Main Coq source code for extend/retract tests during long running Show.
Main part of Coq source code for extend/retract tests during long
running Show without the first Require command, which is in
`coq-src-queuemode-for-show-require'.  When unfolded, the function
build_tree generates big terms that take quite long to print.")


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
goals buffer is not empty afterwards.  If CHECK-RESPONSE-NONEMPTY
is non-nil, additionally check that the response buffer is
non-empty, i.e., shows some message, and is visible in some
window also in two-pane mode."
  (message "goals-after-test: Check goals are present after %s." msg)
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
occurrence of COQ-STM, which should be a regular expression.  At
this point the goals buffer needs to contain something.  Then
assert to the end of COQ-SRC and check that the goals buffer has
been reset.  MSG is used in messages only.  It should say after
which action the goals buffer should have been reset."
  (message "Check that goals are reset after %s." msg)
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
  (goals-after-test coq-src-proof "Proof" nil))

(ert-deftest goals-after-comment ()
  "Test goals are present after a comment."
  (goals-after-test coq-src-comment "comment" nil))

(ert-deftest goals-after-auto ()
  "Test goals are present after ``auto''."
  (goals-after-test coq-src-auto "auto" nil))

(ert-deftest goals-after-simpl ()
  "Test goals are present after ``simpl''."
  (goals-after-test coq-src-simpl "simpl" nil))

(ert-deftest goals-after-error ()
  "Test goals are present after an error."
  (goals-after-test coq-src-error "error" t))

(ert-deftest goals-reset-after-admitted ()
  "The goals buffer is reset after an Admitted."
  (goals-buffer-should-get-reset coq-src-admitted "intros P" "Admitted"))

(ert-deftest goals-reset-no-more-goals ()
  "The goals buffer is reset when there are no more goals."
  (goals-buffer-should-get-reset coq-src-no-more-goals
                                 "Lemma a" "no more goals"))

(ert-deftest goals-reset-qed ()
  "The goals buffer is reset after Qed."
  (goals-buffer-should-get-reset coq-src-qed
                                 "Proof using" "Qed"))

(defun update-goals-when-response (coq-src first-pos goal-2nd msg)
  "Test goals are up-to-date after an error or a command that produces response.
Process COQ-SRC up to the line after the first match of regular
expression FIRST-POS.  At this point the goals buffer should not
be empty.  Process now COQ-SRC up to the end.  If GOAL-2ND is a
regular expression as a string, then check that the goals have
been updated to contain a match for GOAL-2ND.  If GOAL-2ND is no
string, only check that the goals buffer is non-empty.  In any
case, check that the response buffer is not empty and visible in
two-pane mode."
  
  (message "update-goals-when-response: Check goals are updated after %s" msg)
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
  (update-goals-when-response coq-src-update-goal-after-error
                              "Lemma foo"
                              "H : eq_one 1 -> False"
                              "error"))

(ert-deftest goals-up-to-date-after-search-one-step ()
  "Check that goals are still present before showing result of one search cmd.
This test checks a single Search command inside a proof.  After
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
Search command.  After processing these commands, the goals buffer
should have been updated and the response buffer should contain
something and be visible in two-pane mode."
  (update-goals-when-response coq-src-update-goal-after-search
                              "Lemma g"
                              "2 = 2"
                              "Search"))

(ert-deftest goals-up-to-date-after-check-one-step ()
  "Check that goals are still present before showing result of one check cmd.
This test checks a single Check command inside a proof.  After
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
Check command.  After processing these commands, the goals buffer
should have been updated and the response buffer should contain
something and be visible in two-pane mode."
  (update-goals-when-response coq-src-update-goal-after-check
                              "Lemma h"
                              "3 = 3"
                              "Check"))

(defun check-error-at-qed (intermediate-pos)
  "Check that Qed correctly shows an error.
Run a script that provokes an error at Qed about a not declared section
variable and check that the error message is displayed."
  (let (buffer
        (proof-omit-proofs-option nil))
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src-not-declared-section-variable)
          (goto-char (point-min))

          (if intermediate-pos
              (progn
                (message "process up to %s" intermediate-pos)
                (should (re-search-forward intermediate-pos nil t))
                (beginning-of-line)
                (forward-line 1)
                (proof-goto-point)
                (wait-for-coq)
                ;; (record-buffer-content "*coq*")
                )
            (message "process complete script in one step"))

          (goto-char (point-max))
          (message "process complete script to end")
          (proof-goto-point)
          (wait-for-coq)
          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*response*")

          ;; check that the goals buffer is empty
          (with-current-buffer proof-goals-buffer
            (should (equal (point-min) (point-max))))

          (with-current-buffer proof-response-buffer
            (goto-char (point-min))
            (should
             (re-search-forward
              "The following section variable is used but not declared:"
              nil t))))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest error-message-visible-at-qed-complete-script ()
  "Check that the error message is present at the end of the proof.
Run a complete script that provokes an error at Qed about a not declared
section variable and check that the error message is displayed."
  (message "Check that the error message is present at Qed for complete script.")
  (check-error-at-qed nil))

(ert-deftest error-message-visible-at-qed-one-step ()
  "Check that the error message is present for Qed.
Run a proof that uses an undeclared section variable.  Check that the
error message is displayed when running Qed alone as single step."
  (message "Check that the error message is present at Qed for single step.")
  (check-error-at-qed "marker A"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; response buffer visibility tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following tests check that commands such as coq-Search or
;; coq-Check correctly show a response buffer in two-pane mode, even
;; in corner cases such as at the end of the proof or when the
;; response is empty. Many of these commands use the same internal
;; functions such as `coq-ask-do'. Therefore it is sufficiant to test
;; only very few of these functions.
;;
;; The following overview shows which command uses what internally.
;;
;;
;; use only `coq-ask-do': read via `coq-guess-or-ask-for-string'
;;
;; (define-key coq-keymap [(control ?a)]  #'coq-Search)
;; (define-key coq-keymap [(control ?l)]  #'coq-LocateConstant)
;; (define-key coq-keymap [(control ?n)]  #'coq-LocateNotation)
;; (define-key coq-keymap [(control ?o)]  #'coq-SearchIsos)
;;
;;
;; without prefix use `coq-ask-do': read via `coq-guess-or-ask-for-string'
;; with prefix (raw) use `coq-ask-do-show-all', which calls
;; `coq-ask-do-set-unset', also using `coq-guess-or-ask-for-string',
;; finally calling `coq-command-with-set-unset'
;;
;; (define-key coq-keymap [(control ?c)]  #'coq-Check)
;; (define-key coq-keymap [(control ?p)]  #'coq-Print)
;; (define-key coq-keymap [(control ?b)]  #'coq-About)
;; (define-key coq-keymap [(control ?s)]  #'coq-Show)
;;
;;
;; without prefix use `proof-shell-invisible-command'
;; with prefix use `coq-command-with-set-unset' with test command
;;
;; (define-key coq-keymap [(control ?q)]  #'coq-query)
;;
;;
;; via `proof-definvisible', simple `proof-shell-invisible-command'
;;
;; (define-key coq-keymap [?h]            #'coq-PrintHint)
;; (define-key coq-keymap [(control ?9)]  #'coq-set-printing-parentheses)
;; (define-key coq-keymap [(control ?0)]  #'coq-unset-printing-parentheses)
;; (define-key coq-keymap [(?N)]          #'coq-set-printing-notations)
;; (define-key coq-keymap [(?n)]          #'coq-unset-printing-notations)
;;
;;
;; not analyzed or tested
;;
;; (define-key coq-keymap [(control ?w)]  #'coq-ask-adapt-printing-width-and-show)


;; The functions such as `coq-Search' that are tested via
;; `check-response-present' read user input themselves via the
;; minibuffer by using `coq-guess-or-ask-for-string'. For the
;; automated tests we use `advice-add' to replace this function by
;; `replace-coq-guess-or-ask-for-string', which returns the content of
;; `ask-for-string-answer' without reading from the minibuffer.
;; `check-response-present' places the string that shall be returned
;; into `ask-for-string-answer' before calling the function under test.

(defvar ask-for-string-answer ""
  "Place holder for return value of `coq-guess-or-ask-for-string'.
Used inside `check-response-present', see comment above.")

(defun replace-coq-guess-or-ask-for-string (_s &optional _dontguess)
  "Replacement of `coq-guess-or-ask-for-string' for automated tests.
Inside `check-response-present', this function is used as an :override
advice for `coq-guess-or-ask-for-string', such that functions such as
`coq-Search' can be tested without that they read from the minibuffer."
  ask-for-string-answer)

(defun check-response-present (query-fun line input-string response)
  "Check response and visibility of the response buffer.
This function checks that `coq-Search' and similar functions display
their response correctly.  QUERY-FUN is the command to be tested, for
instance `coq-Search', or some closure, if the command needs arguments,
such as `coq-Check'.  LINE is the line number up to which
`coq-src-report-response-check' is processed before QUERY-FUN is called.
INPUT-STRING is the user input that QUERY-FUN shall receive from the
advised `coq-guess-or-ask-for-string'.  RESPONSE is for the content check
of the response buffer.  If RESPONSE is a string, it must be a regular
expression for which a match is searched in the response buffer.  If
RESPONSE is not a string the response buffer must be empty.

Global configuration of this file ensures two-pane mode by setting
`proof-three-window-enable' to nil.  It inserts
`coq-src-report-response-check' into some buffer, processes this up to
line LINE, advises `coq-guess-or-ask-for-string' to return INPUT-STRING,
and calls QUERY-FUN.  It then checks, according to RESPONSE, that the
response buffer is either empty or contains the expected result.  The
function further checks that the response buffer is visible in some
window."
  (let (buffer pos)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src-report-response-check)
          (goto-char (point-min))
          (forward-line (1- line))
          (setq pos (point))
          (proof-goto-point)
          (wait-for-coq)
          (message
           "locked span: %s, locked until %s, point should be at %s, now at %s"
           proof-locked-span
           (and proof-locked-span (span-end proof-locked-span))
           pos (point))
          (should (eq pos (point)))
          (should (and proof-locked-span
                       (span-end proof-locked-span)
                       (eq (1+ (span-end proof-locked-span)) (point))))

          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content "*goals*")

          ;; modify `coq-guess-or-ask-for-string' to return
          ;; `ask-for-string-answer' without reading from the minibuffer.
          (setq ask-for-string-answer input-string)
          (advice-add 'coq-guess-or-ask-for-string :override
                      #'replace-coq-guess-or-ask-for-string)
          (funcall query-fun)
          (advice-remove 'coq-guess-or-ask-for-string
                         #'replace-coq-guess-or-ask-for-string)
          ;; Some tested functions, e.g., `coq-Search', wait for the
          ;; response, others, e.g., `coq-Check', don't wait.
          (wait-for-coq)

          ;; (record-buffer-content "*coq*")
          ;; (record-buffer-content proof-response-buffer)

          (message "Check response buffer content")
          (with-current-buffer proof-response-buffer
            (if (stringp response)
                (progn
                  (goto-char (point-min))
                  (should (re-search-forward response nil t)))
              (should (equal (point-min) (point-max)))))

          (message "Check that the response is visible in two-pane mode")
          (should (get-buffer-window proof-response-buffer)))
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest response-buffer-visible-coq-search-something-inside-proof ()
  "Check response for `coq-Search' on (S (_ + _)) inside proof."
  (message "Check response for Search (S (_ + _)) is shown inside proof")
  (check-response-present #'coq-Search 6 "(S (_ + _))" "^plus_Sn_m: forall"))

(ert-deftest response-buffer-visible-coq-search-something-proof-end ()
  "Check response for `coq-Search' on (S (_ + _)) at proof end.
Skipped for 8.14 and 8.15, there Coq reacts with an error when searching
in proof mode with no more goals."
  (message "Check response for Search (S (_ + _)) is shown at proof end")
  ;; XXX change to skip-when when Emacs 29 is phased out
  (skip-unless (not coq--between-v814-v815))
  (check-response-present #'coq-Search 7 "(S (_ + _))" "^plus_Sn_m: forall"))
      
(ert-deftest response-buffer-visible-coq-search-something-outside-proof ()
  "Check response for `coq-Search' on (S (_ + _)) outside any proof."
  (message "Check response for Search (S (_ + _)) is shown outside proofs")
  (check-response-present #'coq-Search 2 "(S (_ + _))" "^plus_Sn_m: forall"))

(ert-deftest response-buffer-visible-coq-search-empty-inside-proof ()
  "Check empty response for `coq-Search' on 42 inside proof."
  (message "Check empty response for Search 42 is shown inside proof")
  (check-response-present #'coq-Search 6 "42" t))
      
(ert-deftest response-buffer-visible-coq-search-empty-proof-end ()
  "Check empty response for `coq-Search' on 42 at proof end.
Skipped for 8.14 and 8.15, there Coq reacts with an error when searching
in proof mode with no more goals."
  (message "Check empty response for Search 42 at proof end")
  ;; XXX change to skip-when when Emacs 29 is phased out
  (skip-unless (not coq--between-v814-v815))
  (check-response-present #'coq-Search 7 "42" t))

(ert-deftest response-buffer-visible-coq-search-empty-outside-proof ()
  "Check empty response for `coq-Search' on 42 outside proof."
  (message "Check empty response for Search 42 is shown outside proof")
  (check-response-present #'coq-Search 2 "42" t))
      
(ert-deftest response-buffer-visible-coq-check-print-all-inside-poof ()
  "Check response for `coq-Check' on plus_n_Sm inside proof with printing all."
  (message
   "Check response for Check plus_n_Sm proof with printing all")
  (check-response-present
   #'(lambda() (coq-Check t)) 6 "plus_n_Sm" "@eq nat (S (Nat.add"))

(ert-deftest response-buffer-visible-coq-check-print-all-poof-end ()
  "Check response for `coq-Check' on plus_n_Sm at proof end with printing all."
  (message
   "Check response for Check plus_n_Sm at proof end with printing all")
  (check-response-present
   #'(lambda() (coq-Check t)) 7 "plus_n_Sm" "@eq nat (S (Nat.add"))

(ert-deftest response-buffer-visible-coq-check-print-all-outside-poof ()
  "Check response for `coq-Check' on plus_n_Sm outside proof with printing all."
  (message
   "Check response for Check plus_n_Sm outside proof with printing all")
  (check-response-present
   #'(lambda() (coq-Check t)) 2 "plus_n_Sm" "@eq nat (S (Nat.add"))


(defun user-action-during-long-running-show (extend)
  "Test to extend or retract during long running Show.
The source code for this test generates a goal that takes about half a
second to print.  When running completely silent, this printing happens
inside a Show command added as priority item.  The user should be able to
extend the queue region during this long running Show.

This function can test both, extension (if EXTEND is not nil) and
retraction (if EXTEND is nil) during a long running Show.  Retraction
should fail with the error message \"Proof process busy!\". Extending
the queue should not fail.

Process the source code just before the cbv command that produces the
big term. Then process cbv alone but do not wait until Coq finished
processing. Instead, extend or retract after a short delay. Catch
potential errors with `condition-case' and test their error message.

Need to clear `debug-on-error', which is set in ERT in Emacs 29 and
earlier. `debug-on-error' changes `cl-assert' such that it's error is
not handled by `unwind-protect'. Then the next test triggers the wrong
queuemode assertion again, because Coq was not killed in the handler."
  (let (buffer pos)
    (unwind-protect
        (progn
          (find-file "goals.v")
          (setq buffer (current-buffer))
          (insert coq-src-queuemode-for-show-require)
          (insert coq-src-queuemode-for-show-remainder)
          (goto-char (point-min))
          ;; (record-buffer-content (current-buffer))
          (should (re-search-forward "marker A" nil t))
          (forward-line 1)
          (setq pos (point))
          (proof-goto-point)
          (wait-for-coq)
          (message
           "locked span: %s, locked until %s, point should be at %s, now at %s"
           proof-locked-span
           (and proof-locked-span (span-end proof-locked-span))
           pos (point))
          (should (eq pos (point)))
          (should (and proof-locked-span
                       (span-end proof-locked-span)
                       (eq (1+ (span-end proof-locked-span)) (point))))

          (message "Start command with long running Show")
          (forward-line 1)
          (proof-goto-point)
          (accept-process-output nil 0.1)

          ;; (record-buffer-content "*coq*")
          
          (if (consp proof-action-list)
              (progn
                (if extend
                    (progn
                      (message
                       "Show still running, extend queue with next command")
                      (forward-line 1))
                  (message "%s%s"
                           "Show still running, retract queue to line "
                           "before previous command")
                  (forward-line -1))
                
                (condition-case evar
                    (let ((debug-on-error nil))
                      (proof-goto-point))

                  (error
                   ;; If the just excuted proof-goto-point is an
                   ;; retract, then eventually the check in
                   ;; `proof-shell-ready-prover' will raise an error
                   ;; "Proof process busy!". In other cases an
                   ;; cl-assert might get hit, which usually also
                   ;; results in a call to error - just with a
                   ;; different message.
                   (message "Error when extending queue: %s" (cdr evar))
                   ;; Kill Coq here. Otherwise the next test might
                   ;; still find the long running Show.
                   (proof-shell-exit t)
                   (should (equal (cadr evar) "Proof process busy!")))))
            (message "Unexpected: Show not running any more")
            (should nil)))

      ;; clean up
      (when buffer
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest extend-queue-during-long-running-show ()
  "Test extending the queue region during a long running Show."
  :expected-result :failed
  (message "Extend queue during a long running Show of the previous command")
  (user-action-during-long-running-show t))

(ert-deftest retract-during-long-running-show ()
  "Test retracting during a long running Show."
  (message "Retract during a long running Show of the previous command")
  (user-action-during-long-running-show nil))

(provide 'coq-test-goals-present)

;;; coq-test-goals-present.el ends here
