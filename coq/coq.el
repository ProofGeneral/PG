;;; coq.el --- Major mode for Coq proof assistant  -*- coding: utf-8; lexical-binding: t; -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Authors: Healfdene Goguen, Pierre Courtieu
;; Maintainer: Pierre Courtieu <Pierre.Courtieu@cnam.fr>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;

;;; Code:

;; PG doesn't support using several backends at the same time.
;; FIXME: We really should set proof-assistant just because this file is
;; loaded (e.g. we should set it within coq-pg-setup instead), but
;; defpacustom and various others fail if executed before proof-assistant
;; is set.
(if (and (boundp 'proof-assistant)
         (member proof-assistant '(nil "" "Coq" "coq")))
    (proof-ready-for-assistant 'coq)
  (message "Coq-PG error: Proof General already in use for %s" proof-assistant))

(require 'cl-lib)

(require 'span)
(eval-when-compile
  (require 'proof-utils)
  (require 'span)
  (require 'outline)
  (require 'newcomment)
  (require 'etags))
(defvar action)       ; dynamic scope in coq-insert-as stuff
(defvar string)       ; dynamic scope in coq-insert-as stuff
(defvar old-proof-marker)
(defvar coq-keymap)
(defvar coq-one-command-per-line)
(defvar coq-auto-insert-as)             ; defpacustom
(defvar coq-time-commands)              ; defpacustom
(defvar coq-use-project-file)           ; defpacustom
(defvar coq-hide-additional-subgoals)

(require 'proof)
(require 'coq-system)                   ; load path, option, project file etc.
(require 'coq-syntax)                   ; font-lock, syntax categories (tactics, commands etc)
(require 'coq-local-vars)               ; setting coq args via file variables
                                        ;   (prefer _CoqProject file instead)
(require 'coq-abbrev)                   ; abbrev and coq specific menu
(require 'coq-seq-compile)              ; sequential compilation of Requires
(require 'coq-par-compile)              ; parallel compilation of Requires
(require 'coq-diffs)                    ; highlight proof diffs
(require 'coq-goals)                    ; folding of the other goals
(require 'coq-queries)                  ; About, Check, Search etc.
(require 'coq-hyps)                     ; folding and highlighting of hypotheses
(require 'pg-goals-indicators)          ; goals out of view
(require 'pg-double-hit)                ; double hit electric terminator

(defvar prettify-symbols-alist)


;; ----- coq-shell configuration options

;;; Code:
;; debugging functions
;; (defun proofstack () (coq-get-span-proofstack (span-at (point) 'type)))
(defvar coq-debug nil)
;; End debugging

;; Obsolete
;;(defcustom coq-default-undo-limit 500
;;  "Maximum number of Undo's possible when doing a proof."
;;  :type 'number
;;  :group 'coq)

(defcustom coq-run-completely-silent t
  "Run Coq completely silent if enabled.
Please restart Proof General after changing this setting!

If enabled, run Coq completely silent (Set Silent) and only issue
a show command when necessary to update the goals buffer.  This
behavior is new.  If you experience strange effects, please report
a bug and switch this flag off to return to old behavior.  When
disabled, Coq is dynamically switched to silent for longer lists
of commands and switched to verbose before the last command.  A
manual Show command (C-c C-p) is necessary if the last command
fails to update the goals buffer (e.g., if it is a comment or it
is not executed because some other command before failed, see
also bug report #568).  External proof tree display is only
supported if this option is t.

This option is ignored for Rocq >= 9.2, which always runs completely
silent, because it does not print goals any more.  Lisp code should
therefore test `coq--run-silent-p' instead of this option."
  :type 'boolean
  :safe 'booleanp
  :group 'coq)

(defcustom coq-user-init-cmd nil
  "User defined init commands for Coq.
These are appended at the end of `coq-shell-init-cmd'."
  :type '(repeat string)
  :group 'coq)

;; Default coq is only Private_ and _subproof
(defcustom coq-search-blacklist-string ; add this? \"_ind\" \"_rect\" \"_rec\"
 "\"Private_\" \"_subproof\""
  "Initial strings to blacklist in requests to Coq environment.
If you are setting this via Emacs customization menus, you should
restart Coq to see the effect.  To change blacklist during a Coq
development, please use \\[coq-change-search-blacklist-interactive]
instead (or menu: Coq/Settings/Search Blacklist)."
  :type 'string
  :group 'coq)

;; Coq has no command to overwrite the current blacklist, only to add or remove
;; particular strings. To provide the overwrite feature to the user we thus need
;; to know the current value of blacklist to first remove everything, then add
;; the new values.
;;; TODO: retrieve the value from coq itself and remove this variable?
(defvar coq-search-blacklist-current-string coq-search-blacklist-string
  "Current value of Coq Search Blacklist.
This may become desynchronized with Coq if you use the command
\"Add/Remove Search Blacklist\" in you script." )


(defun coq-shell-init-cmd ()
  "Commands for initial Coq configuration, Coq variant of `proof-shell-init-cmd'.
Return the list of commands sent to the Coq background process just
after it has been started.  This happens inside
`proof-shell-config-done', when the major mode `coq-shell-mode' is
configured in the `*coq*' buffer.

This is computed each time Coq is started, after the detection of its
version (see `coq-prog-args'), which may have changed since Proof
General was loaded (e.g., after a change of opam switch).

Sets silent mode if `coq-run-completely-silent' is set and the
version of Coq needs it.

In normal interaction, Coq is started because the user asserts
some commands.  Therefore the commands here are followed by those
inserted inside `proof-assert-command-hook', respectively,
`coq-adapt-printing-width'."
  (append
   `(
     ;; Should this variable be buffer-local? No opinion on that but if yes we
     ;; should re-intialize to coq-search-blacklist-string instead of
     ;; keeping the current value (that may come from another file).
     ,(format "Add Search Blacklist %s." coq-search-blacklist-current-string))
   '("Set Suggest Proof Using.")
   (if (and coq-run-completely-silent
            (coq--version< (coq-version t) "9.2+alpha"))
       '("Set Silent.")
     ())
   coq-user-init-cmd))

;; FIXME: Even if we don't use coq-indent for indentation, we still need it for
;; coq-script-parse-cmdend-forward/backward and coq-find-real-start.
(require 'coq-indent)


;; Command to reset the Coq Proof Assistant
(defconst coq-shell-restart-cmd "Reset Initial.\n ")

(defvar coq-shell-prompt-pattern
  "\\(?:\n\\(?:[^\n\371]+\371\\|<prompt>[^\n]+</prompt>\\)\\)"
  "The prompt pattern for the inferior shell running Coq.")

;; FIXME da: this was disabled (set to nil) -- why?
;; da: 3.5: add experimental
;; am:answer: because of bad interaction
;; with coq -R option.
(defvar coq-shell-cd nil
;;  "Add LoadPath \"%s\"." ;; fixes unadorned Require (if .vo exists).
  "*Command of the inferior process to change the directory.")

(defvar coq-shell-proof-completed-regexp
  (concat "No\\s-+more\\s-+\\(?:sub\\)?goals\\.\\|Subtree\\s-proved!\\|"
          "Proof\\s-completed"
          ;; \\|This subproof is complete
          )
  "*Regular expression indicating that the proof has been completed.
Coq instance of `proof-shell-clear-goals-regexp'.  Used in
`proof-shell-process-urgent-message' to determine if the goals
buffer shall be cleaned.  Some of the messages recognized here are
not printed by Coq in silent mode, such that Proof General might
fail to delete the goals buffer.")

(defvar coq-goal-regexp
  "\\(============================\\)\\|\\(\\(?:sub\\)?goal [0-9]+\\)\n")

(defvar coq-start-goals-regexp
  (concat
   ;; messages about the proof state are displayed with the goals
   "\\(?:<infomsg>\\s-*\\(?:This subproof is complete"
   "\\|All the remaining goals are on the shelf"
   "\\|No more \\(?:sub\\)?goals\\)[^<]*</infomsg>\\s-*\\)?"
   "[0-9]+\\(?: focused\\)? \\(?:sub\\)?goals?"
   "\\|No more \\(?:sub\\)?goals\\.")
  "Coq instance of `proof-shell-start-goals-regexp'.
Messages about the state of the proof (subproof or proof complete,
goals on the shelf or given up) are part of the goals output, so that
they are displayed in the goals buffer instead of the response buffer.")


(defconst coq-interrupt-regexp "User Interrupt."
  "Regexp corresponding to an interrupt.")

(defcustom coq-end-goals-regexp-show-subgoals "\n(dependent evars:"
  "Regexp for `proof-shell-end-goals-regexp' when showing all subgoals.
A setting of nil means show all output from Coq.  See also option
`coq-hide-additional-subgoals'."
  :type '(choice regexp (const nil))
  :group 'coq)

(defcustom coq-end-goals-regexp-hide-subgoals
  (concat "\\(\n\\(?:sub\\)?goal 2 \\)\\|\\(" coq-end-goals-regexp-show-subgoals "\\)")
  "Regexp for `proof-shell-end-goals-regexp' when hiding additional subgoals.
See also option `coq-hide-additional-subgoals'."
  :type '(choice regexp (const nil))
  :group 'coq)

(defcustom coq-show-proof-stepwise nil
  "Display the proof terms stepwise in the *response* buffer.
This option can be combined with option `coq-diffs'.
It is mostly useful in three window mode, see also
`proof-three-window-mode-policy' for details."

  :type 'boolean
  :safe 'booleanp
  :group 'coq-auto-compile)

;;
;; prooftree customization
;;

(defgroup coq-proof-tree ()
  "Coq specific customization for prooftree."
  :group 'coq-config
  :package-version '(ProofGeneral . "4.2"))

(defcustom coq-proof-tree-ignored-commands-regexp
  "^\\(\\(Show\\)\\|\\(Locate\\)\\|\\(Proof\\)\\)"
  "Regexp for `proof-tree-ignored-commands-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-navigation-command-regexp
  (concat "^\\(\\(Focus\\)\\|\\(Unfocus\\)\\|"
          "\\(all\\s-*:\\s-*\\(cycle\\|swap\\|revgoals\\)\\)\\|"
          "\\(\\+\\)\\|\\(-\\)\\|\\(\\*\\)\\|"
          "\\(\\([0-9]+\\s-*:\\s-*\\)?{\\)\\|"
          "\\(}\\)\\)")
  "Regexp for `proof-tree-navigation-command-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-cheating-regexp
  "\\(?:admit\\)\\|\\(?:give_up\\)"
  "Regexp for `proof-tree-cheating-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-current-goal-regexp
  (concat "^[0-9]+ \\(?:focused \\)?\\(?:sub\\)?goal\\(?:s\\)?\\s-*"
          "\\(?:(\\(?:unfocused: [-0-9]+\\)?,?"
          "\\s-*\\(?:shelved: [-0-9]+\\)?)\\)?\\(?:\\s-*, \\(?:sub\\)?goal 1\\)? "
          "(ID \\([0-9]+\\))\n\\s-*\n\\(\\(?: .*\n\\)+\\)\\(?:\n\\|$\\)")
  "Regexp for `proof-tree-current-goal-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-update-goal-regexp
  (concat "^goal ID \\([0-9]+\\) at state \\([0-9]+\\)\n"
          "\\s-*\n\\(\\(?:.+\n\\)*\\)\\(?:\n\\|$\\)")
  "Regexp for `proof-tree-update-goal-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-additional-subgoal-ID-regexp
  "^\\(?:sub\\)?goal [0-9]+ (ID \\([0-9]+\\)) is:"
  "Regexp for `proof-tree-additional-subgoal-ID-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-manage-dependent-evar-line t
  "Switch dependent evar line on and off for proof tree displayed proofs."
  :type 'boolean
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-existentials-state-start-regexp
  "^(dependent evars:"
  "Coq instance of `proof-tree-existentials-state-start-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-existentials-state-end-regexp ")\n"
  "Coq instance of `proof-tree-existentials-state-end-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

;; 8.4:
;; <infomsg>This subproof is complete, but there are still unfocused goals.</infomsg>
;;
;; 8.5:
;; <infomsg>
;; This subproof is complete, but there are some unfocused goals.
;; Focus next goal with bullet *.
;; </infomsg>
;;
;; <infomsg>No more subgoals, but there are some goals you gave up:</infomsg>
;;
;; <infomsg>All the remaining goals are on the shelf.</infomsg>
(defcustom coq-proof-tree-branch-finished-regexp
  (concat "^\\(\\(?:Proof completed\\.\\)\\|"
          "\\(?:\\(?:<infomsg>\\)?No more \\(?:sub\\)?goals\\)\\|"
          "\\(No more \\(?:sub\\)?goals but non-instantiated "
          "existential variables:\\)\\|"
          "\\(?:<infomsg>All the remaining goals are on the shelf\\)\\|"
          "\\(<infomsg>\\s-*This subproof is complete, but there are "
          "\\(?:still\\|some\\) unfocused goals.\\)\\)")
  "Regexp for `proof-tree-branch-finished-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)


;;
;; Outline mode
;;

(defvar coq-shell-outline-regexp coq-goal-regexp)
(defvar coq-shell-outline-heading-end-regexp coq-goal-regexp)


(defconst coq-state-preserving-tactics-regexp
  (proof-regexp-alt-list coq-state-preserving-tactics))
(defconst coq-state-changing-commands-regexp
  (proof-regexp-alt-list coq-keywords-state-changing-commands))
(defconst coq-state-preserving-commands-regexp
  (proof-regexp-alt-list coq-keywords-state-preserving-commands))
(defconst coq-commands-regexp
  (proof-regexp-alt-list coq-keywords-commands))
(defvar coq-retractable-instruct-regexp
  (proof-regexp-alt-list coq-retractable-instruct))
(defvar coq-non-retractable-instruct-regexp
  (proof-regexp-alt-list coq-non-retractable-instruct))


;;
;; Derived modes
;;

(defvar coq-shell-mode-syntax-table coq-mode-syntax-table)
(defvar coq-response-mode-syntax-table coq-mode-syntax-table)
(defvar coq-goals-mode-syntax-table coq-mode-syntax-table)

(define-derived-mode coq-shell-mode proof-shell-mode
  "Coq Shell" nil
  (coq-shell-mode-config))

(define-derived-mode coq-response-mode proof-response-mode
  "Coq Response" nil
  (coq-response-config))
(define-derived-mode coq-goals-mode proof-goals-mode
  "Coq Goals" nil
  (coq-goals-mode-config))

;;
;; Auxiliary code for Coq modes
;;

;; finding the main frame of pg.
(defun coq-find-threeb-frames ()
  "Return a list of frames displaying both response and goals buffers."
  (let* ((wins-resp (get-buffer-window-list proof-response-buffer nil t))
         (wins-gls (get-buffer-window-list proof-goals-buffer nil t))
         (frame-resp (mapcar #'window-frame wins-resp))
         (frame-gls (mapcar #'window-frame wins-gls)))
    (filtered-frame-list (lambda (x) (and (member x frame-resp) (member x frame-gls))))))


;; slight modification of proof-script-generic-parse-cmdend (one of the
;; candidate for proof-script-parse-function). See its definition in
;; coq-indent.el.
(defun coq-script-parse-function ()
  "For `proof-script-parse-function' if `proof-script-command-end-regexp' set."
  (coq-script-parse-cmdend-forward))


;; The frozen copies of the goals and response buffers are in coq-goals.el.



;; make this non recursive?
(defun build-list-id-from-string (s)
  "Build a list of string from a string S of the form \"id1|id2|...|idn\"."
  (if (or (not s) (string= s "")) '()
    (let ((x (string-match (concat "\\(" coq-id-shy "\\)\\(?:|\\|\\'\\)\\(.*\\)") s)))
      (if (not x) (error "Cannot extract list of ids from string")
        (cons (match-string 1 s)
              (build-list-id-from-string (match-string 2 s)))))))

;; Use the statenumber inside the coq prompt to backtrack more easily
(defun coq-last-prompt-info (s)
  "Extract info from the Coq prompt S.  See `coq-last-prompt-info-safe'."
  (let ((lastprompt (or s (error "No prompt !!?")))
        (regex
         (concat ">\\(" coq-id-shy "\\) < \\([0-9]+\\) |\\(\\(?:" coq-id-shy
                 "|?\\)*\\)| \\([0-9]+\\) < ")))
    (when (string-match regex lastprompt)
      (let ((current-proof-name (match-string 1 lastprompt))
            (state-number (string-to-number (match-string 2 lastprompt)))
            (proof-state-number (string-to-number (match-string 4 lastprompt)))
            ;; bind pending-proofs last, because build-list-id-from-string
            ;; modifies the match data
            (pending-proofs
             (build-list-id-from-string (match-string 3 lastprompt))))
        (list state-number proof-state-number pending-proofs
              (if pending-proofs current-proof-name nil))))))


(defun coq-last-prompt-info-safe ()
  "Return a list with all information from the last prompt.
The list contains in the following order the state number, the
proof stack depth, a list with the names of all pending proofs,
and as last element the name of the current proof (or nil if
there is none)."
  (coq-last-prompt-info proof-shell-last-prompt))

(defvar coq-last-but-one-statenum 1
  "The state number we want to put in a span.
This is the prompt number given *just before* the command was sent.
This variable remembers this number and will be updated when
used see coq-set-state-number.
Initially 1 because Coq initial state has number 1.")

(defvar coq-last-but-one-proofnum 1
  "As for `coq-last-but-one-statenum' but for stack depth.")

(defvar coq-last-but-one-proofstack '()
  "As for `coq-last-but-one-statenum' but for proof stack symbols.")

(defvar coq--retract-naborts 0
  "The number of (possibly nested) goals to abort on retract.
This temporary variable is written by function `coq-find-and-forget'
and read by function `coq-empty-action-list-command'.")

(defsubst coq-get-span-statenum (span)
  "Return the state number of the SPAN."
  (span-property span 'statenum))

(defsubst coq-get-span-proofnum (span)
  "Return the proof number of the SPAN."
  (span-property span 'proofnum))

(defsubst coq-get-span-proofstack (span)
  "Return the proof stack (names of pending proofs) of the SPAN."
  (span-property span 'proofstack))

(defsubst coq-set-span-statenum (span val)
  "Set the state number of the SPAN to VAL."
  (span-set-property span 'statenum val))

(defsubst coq-get-span-goalcmd (span)
  "Return the \\+`goalcmd' flag of the SPAN."
  (span-property span 'goalcmd))

(defsubst coq-set-span-goalcmd (span val)
  "Set the \\+`goalcmd' flag of the SPAN to VAL."
  (span-set-property span 'goalcmd val))

(defsubst coq-set-span-proofnum (span val)
  "Set the proof number of the SPAN to VAL."
  (span-set-property span 'proofnum val))

(defsubst coq-set-span-proofstack (span val)
  "Set the proof stack (names of pending proofs) of the SPAN to VAL."
  (span-set-property span 'proofstack val))

(defsubst proof-last-locked-span ()
  (with-current-buffer proof-script-buffer
    (span-at (- (proof-unprocessed-begin) 1) 'type)))

;; Each time the state changes (hook below), (try to) put the state number in
;; the last locked span (will fail if there is already a number which should
;; happen when going back in the script).  The state number we put is not the
;; last one because the last one has been sent by Coq *after* the change. We
;; use `coq-last-but-one-statenum' instead and then update it.

;;TODO update docstring and comment

(defun coq-open-goals-p ()
  "Return non nil if we are inside a proof, see `proof-shell-open-goals-p'."
  (car (cdr (cdr (coq-last-prompt-info-safe)))))


(defun coq-set-state-infos ()
  "Set the last locked span's state number to the number found last time.
This number is in the *last but one* prompt (variable `coq-last-but-one-statenum').
If locked span already has a state number, then do nothing.  Also updates
`coq-last-but-one-statenum' to the last state number for next time."
  (if proof-shell-last-prompt
      ;; da: did test proof-script-buffer here, but that seems wrong
      ;; since restart needs to reset these values.
      ;; infos = promt infos of the very last prompt
      ;; sp = last locked span, which we want to fill with prompt infos
      (let ((sp    (if proof-script-buffer (proof-last-locked-span)))
            (infos (or (coq-last-prompt-info-safe)
                       ;; the line above seems to return nil sometimes, let us
                       ;; issue a warning when this happens, so that we
                       ;; understand why.
                       (and (display-warning
                             'proof-general
                             "oops nothing returned by (coq-last-prompt-info-safe)!!!" :debug) nil)
                       '(0 0 nil nil))))
        (unless (or (not sp) (coq-get-span-statenum sp))
          (coq-set-span-statenum sp coq-last-but-one-statenum))
        (setq coq-last-but-one-statenum (car infos))
        ;; set goalcmd property if this is a goal start
        ;; (ie proofstack has changed and not a save cmd)
        (unless
            (or (not sp) (equal (span-property sp 'type) 'goalsave)
                (<= (length (car (cdr (cdr infos))))
                    (length coq-last-but-one-proofstack)))
          (coq-set-span-goalcmd sp t))
        ;; testing proofstack=nil is not good here because nil is the empty list OR
        ;; the no value, so we test proofnum as it is always set at the same time.
        ;; This is why this test is done before the next one (which sets proofnum)
        (unless (or (not sp) (coq-get-span-proofnum sp))
          (coq-set-span-proofstack sp coq-last-but-one-proofstack))
        (setq coq-last-but-one-proofstack (car (cdr (cdr infos))))
        (unless (or (not sp) (coq-get-span-proofnum sp))
          (coq-set-span-proofnum sp coq-last-but-one-proofnum))
        (setq coq-last-but-one-proofnum (car (cdr infos))))))

;; This hook seems the one we want.
;; WARNING! It is applied once after each command PLUS once before a group of
;; commands is started
(add-hook 'proof-state-change-pre-hook #'coq-set-state-infos)


(defun count-not-intersection (l notin)
  "Return the number of elts of L that are not in NOTIN."
  (let ((l1 l) (l2 notin) (res 0))
    (while l1
      (if (member (car l1) l2) ()
        (setq res (+ res 1))) ; else
      (setq l1 (cdr l1)))
    res
    ))

;; Simplified version of backtracking which uses state numbers (Coq >= 8.4).
;; It uses the new coq command
;; "BackTo". The prompt is like this:
;;      state                        proof stack
;;      num                           depth
;;       __                              _
;; aux < 12 |aux|SmallStepAntiReflexive| 4 < \371
;; ^^^^^^   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^     ^
;; usual           pending proofs           usual
;;                                          special char
;; exemple:
;; to go (back) from 12 |lema1|lema2...|leman| xx
;; to                8  |lemb1|lemb2...|lembm| 5
;; we must do "BackTo 8" and set `coq--retract-naborts' to
;; naborts, i.e., the number of lemais that are not lembis

;; Rem: We could deal with suspend and resume with more work. We would need a new coq
;; command, because it is better to backtrack with *one* command (because
;; proof-change-hook used above is not exactly called at right times).

(defun coq-find-and-forget (span)
  "Backtrack to SPAN.  Using the \"BackTo n\" Coq command."
  (if (eq (span-property span 'type) 'proverproc)
      ;; processed externally (i.e. Require, etc), nothing to do
      ;; (should really be unlocked when we undo the Require).
      nil
    (let* (;; (naborts 0)
           (proofdepth (coq-get-span-proofnum span))
           (proofstack (coq-get-span-proofstack span))
           (span-staten (coq-get-span-statenum span))
           (naborts (count-not-intersection
                     coq-last-but-one-proofstack proofstack)))
      ;; if we move outside of any proof, coq does not print anything, so clean
      ;; the goals buffer otherwise the old one will still be displayed
      (if (= proofdepth 0) (proof-clean-buffer proof-goals-buffer))
      (setq coq--retract-naborts naborts)
      (list
       (format "BackTo %s ."
               (int-to-string span-staten))))))

(defvar coq-current-goal 1
  "Last goal that Emacs looked at.")

(defun coq-goal-hyp ()
  "Instantiation for `pg-topterm-goalhyplit-fn', see there."
  (cond
   ((looking-at "============================\n")
    (goto-char (match-end 0))
    (cons 'goal (int-to-string coq-current-goal)))
   ((looking-at "\\(?:sub\\)?goal \\([0-9]+\\) is:\n")
    (goto-char (match-end 0))
    ;; (cons 'goal (match-string 1)) ;FIXME: This is dead-code!?  --Stef
    (setq coq-current-goal (string-to-number (match-string 1))))
   ((proof-looking-at proof-shell-assumption-regexp)
    (cons 'hyp (match-string 1)))
   (t nil)))

(defun coq-state-preserving-p (cmd)
  "Instantiation for `proof-state-preserving-p', see there."
  ;; (or
   (proof-string-match coq-non-retractable-instruct-regexp cmd))
  ;; (and
  ;;  (not (proof-string-match coq-retractable-instruct-regexp cmd))
  ;;  (or
  ;;   (message "Unknown command, hopes this won't desynchronize ProofGeneral")
  ;;   t))))

(defun coq-cmd-prevents-proof-omission (cmd)
  "Instantiation for `proof-script-cmd-prevents-proof-omission'.
This predicate decides whether a command inside a proof might
have effects outside the proof, which would prohibit omitting the
proof, see `proof-script-omit-proofs'.

Commands starting lower case are deemed as tactics that have
proof local effect only and so are bullets and braces.  Everything
else is checked against the STATECH field in the Coq syntax data
base, see coq-db.el."
  (if (or (proof-string-match coq-lowercase-command-regexp cmd)
          (proof-string-match coq-bullet-regexp cmd)
          (proof-string-match coq-braces-regexp cmd))
      nil
    (not (coq-state-preserving-p cmd))))

(defun coq-hide-additional-subgoals-switch ()
  "Function invoked when the user switches option `coq-hide-additional-subgoals'."
  (if coq-time-commands
      (progn
        (setq coq-hide-additional-subgoals nil)
        (error
         "You must disable ``Time Commands'' (var coq-time-commands) first"))
    (if coq-hide-additional-subgoals
        (setq proof-shell-end-goals-regexp coq-end-goals-regexp-hide-subgoals)
      (setq proof-shell-end-goals-regexp coq-end-goals-regexp-show-subgoals))))

(defun coq-time-commands-switch ()
  "Function invoked when the user switches option `coq-time-commands'.
Reset option `coq-hide-additional-subgoals' and puts nil into
`proof-shell-end-goals-regexp' to ensure the timing is visible in
the *goals* buffer."
  (if coq-time-commands
      (progn
        (let ((coq-time-commands nil))
          (customize-set-variable 'coq-hide-additional-subgoals nil))
        (setq proof-shell-end-goals-regexp nil))
    (coq-hide-additional-subgoals-switch)))

;;
;; Commands for Coq
;;

(defun coq-flag-is-on-p (testcmd)
  (proof-shell-ready-prover)
  (proof-shell-invisible-command (format testcmd) 'wait)
  (let ((resp proof-shell-last-response-output))
    (string-match "is on\\>" resp)))

(defun coq-command-with-set-unset (setcmd cmd unsetcmd &optional postformatcmd
                                          testcmd)
  "Play commands SETCMD then CMD and then silently UNSETCMD.
The last UNSETCMD is performed with tag \\+`empty-action-list' so that it
does not trigger `proof-shell-empty-action' (which does \"Show\" at
the time of writing this documentation). Also add
`'dont-show-when-silent' everywhere to suppress show commands when
running silent."
  (let* ((postform (if (eq postformatcmd nil) 'identity postformatcmd))
         (flag-is-on (and testcmd (coq-flag-is-on-p testcmd))))
    ;; We put 'empty-action-list tags on all three commands since we don't want
    ;; to trigger "Show" or anything that we usually insert after a group of
    ;; commands.
    (unless flag-is-on (proof-shell-invisible-command
                        (format " %s ." (funcall postform setcmd))
                        nil
                        nil
                        'no-response-display
                        'empty-action-list
                        'dont-show-when-silent))
    (proof-shell-invisible-command
     (format " %s ." (funcall postform cmd))
     'wait
     nil
     'empty-action-list 'dont-show-when-silent)
    (unless flag-is-on (proof-shell-invisible-command
                        (format " %s ." (funcall postform unsetcmd))
                        'waitforit
                        nil
                        'no-response-display
                        'empty-action-list
                        'dont-show-when-silent))))

(defun coq-pg--flag-opposite (flag)
  "The command undoing FLAG: Unset for Set and conversely."
  (if (string-match "\\`Set " flag)
      (replace-match "Unset " t t flag)
    (replace-regexp-in-string "\\`Unset " "Set " flag t t)))

(defun coq-pg--flag-in-effect-p (flag)
  "Non-nil if FLAG, like \"Set Printing All\", is already in effect.
Ask Coq with the corresponding Test command."
  (let ((on (coq-flag-is-on-p
             (replace-regexp-in-string "\\`\\(Un\\)?[Ss]et " "Test " flag t t))))
    (if (string-match "\\`Set " flag) on (not on))))

(defun coq-pg-query (command &optional flags then)
  "Send the query COMMAND to Coq with printing FLAGS, then call THEN.
The flags not in effect yet are set before the query and unset after
it.  This is `coq-query-function' for Proof General, see it for the
arguments."
  (proof-shell-ready-prover)
  (let ((flags (cl-remove-if #'coq-pg--flag-in-effect-p flags)))
    ;; As in `coq-command-with-set-unset': with flags, no Show or
    ;; anything usually inserted after a group of commands.
    (dolist (flag flags)
      (proof-shell-invisible-command
       (concat flag " .") nil nil
       'no-response-display 'empty-action-list 'dont-show-when-silent))
    (apply #'proof-shell-invisible-command
           (concat command " .") (or flags then) nil
           (if flags '(empty-action-list dont-show-when-silent)
             '(dont-show-when-silent)))
    (dolist (flag (reverse flags))
      (proof-shell-invisible-command
       (concat (coq-pg--flag-opposite flag) " .") 'waitforit nil
       'no-response-display 'empty-action-list 'dont-show-when-silent)))
  (when then (funcall then)))

(defun coq-pg-query-result (command)
  "Send the query COMMAND to Coq and return its answer, nil on an error.
This is `coq-query-result-function' for Proof General, see it."
  (proof-shell-ready-prover)
  (let ((output (proof-shell-invisible-cmd-get-result (concat command " ."))))
    (unless (proof-string-match coq-error-regexp output)
      output)))

  ;; (let* ((cmd) (postform (if (eq postformatcmd nil) 'identity postformatcmd)))


  ;;   (proof-shell-ready-prover)
  ;;   (setq cmd (coq-guess-or-ask-for-string ask dontguess))
  ;;   (coq-command-with-set-unset
  ;;    "Set Printing Implicit"
  ;;    (format (concat do " %s .") cmd)
  ;;    "Unset Printing Implicit" )
  ;;   ))


(defsubst coq-put-into-brackets (s)
  (concat "[ " s " ]"))

(defsubst coq-put-into-double-quote-if-notation (s)
  (if (coq-is-symbol-or-punct (string-to-char s))
      (concat "\"" s "\"")
    s))


(defun coq-build-removed-pattern (s)
  (concat " -\"" s "\""))

(defun coq-build-removed-patterns (l)
  (mapcar #'coq-build-removed-pattern l))

(defsubst coq-put-into-quotes (s)
  (concat "\"" s "\""))

(defun coq-set-undo-limit (undos)
  (proof-shell-invisible-command (format "Set Undo %s ." undos)))

(defun coq-get-response-string-at (&optional pt)
  "Go forward from PT until reaching a \\+`response' property, and return it.
Response span only starts at first non space character of a
command, so we may have to go forward to find it.  Starts
from (point) if pt is nil.  Precondition: pt (or point if nil)
must be in locked region."
  (let ((pt (or pt (point))))
    (save-excursion
      (goto-char pt)
      (while (and (not (eq (point) (point-max)))
                  (not (span-at (point) 'response)))
        (forward-char))
      (span-property (span-at (point) 'response) 'response))))

(defun coq-pg-show-at-point ()
  "Show the answer of Coq to the command at point, in the locked region.
Return nil outside of it.  This is `coq-show-at-point-function' for
Proof General, see it."
  ;; This relies on the \\+`response' property, empty when the command was
  ;; processed silently.  We should first have a coq command asking to
  ;; print the goal at a given state.
  (when (proof-in-locked-region-p)
    (let ((s (coq-get-response-string-at)))
      (if (zerop (length s))
          (message "Cannot show the state at this point: Coq was silent during this command.")
        (set-buffer proof-response-buffer)
        (let ((inhibit-read-only 'titi))
          (pg-response-display s)
          (proof-display-and-keep-buffer proof-response-buffer)
          (coq-optimise-resp-windows))))
    t))

;; Since Printing Width is a synchronized option in coq (?) it is retored
;; silently to a previous value when retracting. So we reset the stored width
;; when retracting, so that it will be auto-adapted at the next command. Not
;; perfect: we have to forward one step to see the effect.

;; FIXME: hopefully this will eventually become a non synchronized option and
;; we can remove this.
;; This obeys coq-auto-adapt-printing-width
(add-hook 'proof-assert-command-hook #'coq-adapt-printing-width)
(add-hook 'proof-retract-command-hook #'coq-reset-printing-width)

(defun coq--show-proof-stepwise-cmds ()
  (when coq-show-proof-stepwise
    (if (coq--post-v811)
        (pcase coq-diffs
          ('off     '("Show Proof."))
          ('on      '("Show Proof Diffs."))
          ('removed '("Show Proof Diffs removed.")))
      '("Show Proof."))))

;; In case of nested proofs (which are announced as obsolete in future versions
;; of coq) Coq does not show the goals of enclosing proof when closing a nested
;; proof. This is coq's proof-shell-empty-action-list-command function which
;; inserts a "Show" if the last command of an action list is a save command and
;; there is more than one open proof before that save. If you want to issue a
;; command and *not* have the goal redisplayed, the command must be tagged with
;; 'empty-action-list.
(defun coq-empty-action-list-command (cmd)
  "Return the list of commands to send to Coq if CMD is last in the action list.
Return the list of commands to be sent to Coq when
`proof-action-list' is empty, CMD was the last command just sent
to Coq and CMD was not tagged with `'empty-action-list'.
Note: the last command added if `coq-show-proof-stepwise' is set
should match the `coq-show-proof-diffs-regexp'.

This function is called from `proof-shell-exec-loop' via
`proof-shell-empty-action-list-command'."
  (cond
   ((or
     ;; If closing a nested proof, Show the enclosing goal.
     (and (string-match-p coq-save-command-regexp-strict cmd)
          (> (length coq-last-but-one-proofstack) 1))
     ;; If user issued a printing option then t printing.
     (and (string-match-p "\\(S\\|Uns\\)et\\s-+Printing" cmd)
          (> (length coq-last-but-one-proofstack) 0)))
    `("Show."
      . ,(coq--show-proof-stepwise-cmds)))
   
   ((or
     ;; If we go back in the buffer and the number of abort is less than the
     ;; number of nested goals, that is, if we are inside a proof, then Show
     ;; the goal.
     (and (string-match-p "BackTo\\s-" cmd)
          (> (length coq-last-but-one-proofstack) coq--retract-naborts)))
    (append
     (unless (coq--run-silent-p) '("Unset Silent."))
     (if (coq--post-v810) (list (coq-diffs)) ())
     (coq--show-proof-stepwise-cmds)))

   ((or
     ;; If we go back in the buffer and not in the above case, i.e., outside a
     ;; proof, then only set the Diffs option.
     (string-match-p "BackTo\\s-" cmd))
    (append
     (unless (coq--run-silent-p) '("Unset Silent."))
     (if (coq--post-v810) (list (coq-diffs)) ())))
     
   ((or
     ;; If starting a proof, Show Proof if need be
     (coq-goal-command-str-p cmd)
     ;; If doing (not closing) a proof, Show Proof if need be
     (and (not (string-match-p coq-save-command-regexp-strict cmd))
          (> (length coq-last-but-one-proofstack) 0)))
     (coq--show-proof-stepwise-cmds))))

;; This does not Set Printing Width, it rather tells pg to do that before each
;; command (if necessary).  The option `coq-auto-adapt-printing-width' is
;; common to all backends, see coq-goals.el.  Here only its entry in the
;; settings menu, as `defpacustom' would do.
(unless (assq 'auto-adapt-printing-width proof-assistant-settings)
  (push (list 'auto-adapt-printing-width nil 'boolean
              (documentation-property 'coq-auto-adapt-printing-width
                                      'variable-documentation t))
        proof-assistant-settings))

(defvar coq-shell-current-line-width nil
  "Current line width of the Coq printing width.
Its value will be updated whenever a command is sent if
necessary.")

;; Resetting the known printing width (for when we don't know it, for example
;; when retracting.
(defun coq-reset-printing-width ()
  (setq coq-shell-current-line-width nil))

(defun coq-buffer-window-width (buffer)
  "Return the width of a window currently displaying BUFFER."
  (let*
      ((buf-wins (get-buffer-window-list buffer nil t))
       (_ (if (not (eq 1 (length buf-wins)))
              (display-warning
               'proof-general
               "Zero or more than one goals window, guessing window width."
               :debug)))
       (buf-win (car buf-wins)));; TODO return the widest one instead of the first?
    ;; return nil if no goal buffer found
    (and buf-win (window-width buf-win))))


(defun coq-goals-window-width ()
  (coq-buffer-window-width proof-goals-buffer))
(defun coq-response-window-width ()
  (coq-buffer-window-width proof-response-buffer))

(defun coq-guess-goal-buffer-at-next-command ()
  "Return the probable width of goals buffer if it pops up now.
This is a guess based on the current width of goals buffer if
present, current pg display mode and current geometry otherwise."
  (let ((pol (proof-guess-3win-display-policy proof-three-window-mode-policy)))
    (cond
     ;; goals buffer is visible, bingo
     ((coq-goals-window-width))
     ;; Below is the heuristic to guess the good width for goals
     ;; 2 windows mode, response-buffer visible, use this width
     ((and (not proof-three-window-enable) (coq-response-window-width)))
     ;; 2 windows mode, response buffer not visible, give up
     ((not proof-three-window-enable) nil)
     ;; 3 windows mode, one frame, and vertical policy
     ((and proof-three-window-enable (not proof-multiple-frames-enable)
           (eq pol 'vertical))
      (window-width))
     ;; 3 windows mode, one frame, other policies, give up
     ((and proof-three-window-enable (not proof-multiple-frames-enable))
      nil)
     ;; multiple frames. If goals buffer pops up it will have frame default
     ;; size. Falling back to X default window size if not specified.
     ;; This is hard to mimick, let us give up
     (proof-multiple-frames-enable nil)
     (t nil) ;; assert false?
     )))

(defun coq-adapt-printing-width (&optional show width)
  "Sends a Set Printing Width command to Coq to fit the response window's WIDTH.
A Show command is also issued if SHOW is non-nil, so that the goal is
redisplayed."
  (interactive)
  (when coq-auto-adapt-printing-width
    (let ((wdth (or width (coq-guess-goal-buffer-at-next-command))))
      ;; if no available width, or unchanged, do nothing
      (when (and wdth (not (equal wdth coq-shell-current-line-width)))
        (proof-shell-invisible-command
         (format "Set Printing Width %S." (- wdth 1))
         t)
        (setq coq-shell-current-line-width wdth)
        ;; Show iff show non nil and some proof is under way
        (when (and show (not (null (cl-caddr (coq-last-prompt-info-safe)))))
          (proof-shell-invisible-command (format "Show.") t nil 'no-error-display))))))

(defun coq-adapt-printing-width-and-show (&optional _show width)
  (interactive)
  (coq-adapt-printing-width t width))

(defun coq-ask-adapt-printing-width-and-show ()
  (interactive)
  (let* ((deflt (coq-goals-window-width))
         (rd (read-number (format "Width (%S): " deflt) deflt)))
    (coq-adapt-printing-width t rd)))

;;;;; utils fo overlays
(defun coq-overlays-at (pt prop)
  (let ((overlays (overlays-at pt))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))


(defun coq-pg-set-flag (command)
  "Send COMMAND, setting or unsetting a printing flag, to Coq.
This is `coq-set-flag-function' for Proof General, see it."
  (proof-shell-invisible-command (concat command ".")))

;; The commands setting the printing flags are in coq-queries.el.
; Takes an argument
;(proof-definvisible coq-set-printing-printing-depth "Set Printing Printing Depth .")
;(proof-definvisible coq-unset-printing-printing-depth "Unset Printing Printing Depth .")

;; Persistent setting, non-boolean, non cross-version compatible (Coq >= 8.10)
(defconst coq-diffs--function #'coq-diffs
  "Symbol corresponding to the function `coq-diffs'.
Required to benefit from delayed evaluation when
`proof-assistant-format-lambda' calls (funcall coq-diffs--function)
at `proof-assistant-settings-cmds' evaluation time.")
(add-to-list 'proof-assistant-settings
             '(diffs--function "%l" string "Show Diffs in Coq") t)

(defun coq-Compile ()
  "Compiles current buffer."
  (interactive)
  (let* ((n (buffer-name))
         (l (string-match ".v" n)))
    (compile (concat "make " (substring n 0 l) ".vo"))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Configuring proof and pbp mode and setting up various utilities  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; General consensus among users: flickering spans are much too annoying
;; compared to the usefulness of tooltips.
;; Set to t to bring it back%%
;;
;; FIXME: this always sets proof-output-tooltips to nil, even if the user puts
;; explicitly the reverse in it sconfig file. I just want to change the
;; *default* value to nil.
(custom-set-default 'proof-output-tooltips nil)

;;;;;;;;;;;;;;;;;;;;;;;attempt to deal with debug mode ;;;;;;;;;;;;;;;;

;; tries to extract the last debug goal and display it in goals buffer
(defun coq-display-debug-goal ()
  (interactive)
  (with-current-buffer proof-shell-buffer
    (goto-char (point-max))
    (let ((pt (save-excursion (forward-line -1) (point)))
          (last-prompt (save-excursion (forward-line -1)
                                       (re-search-backward "</prompt>" nil t))))
      (save-excursion
        (when (and
               (re-search-backward "^TcDebug" pt t) ;; current TcDebug
               (re-search-backward "^TcDebug" last-prompt t)) ;; previous one
               (re-search-forward "^Goal:" pt t))
        (beginning-of-line)
        (let ((pt2 (point)))
          (re-search-forward "</prompt>\\|^Debug:\\|^Going to execute:\\|^TcDebug" nil t)
          (goto-char (match-beginning 0))
          (pg-goals-display (buffer-substring pt2 (point)) nil nil)
          (beginning-of-line)
          (pg-response-message (buffer-substring (point) (point-max)))
          )))))

;; overwrite the generic one, interactive prompt is Debug mode;; try to display
;; the debug goal in the goals buffer.
(defun proof-shell-process-interactive-prompt-regexp ()
  "Action taken when `proof-shell-interactive-prompt-regexp' is observed."
  (when (proof-shell-live-buffer)
    (switch-to-buffer proof-shell-buffer)
    (coq-display-debug-goal)
    (message "Prover expects input in %s buffer (if debug mode: h<return> for help))" proof-shell-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar coq-pg-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map proof-mode-map)
    ;; The terminator is bound in `coq-mode-map', for all backends.  This
    ;; overrides the following in proof-script:
    ;; (define-key proof-mode-map (vector (aref proof-terminal-string 0))
    ;;             #'proof-electric-terminator)
    map))

(defvar coq--run-silent-warned nil
  "Non-nil if the user has been warned that Rocq >= 9.2 runs silent.")

(defun coq--run-silent-p ()
  "Return non-nil if Coq runs completely silent.
This is the value of `coq-run-completely-silent', except for Rocq >=
9.2, which always runs completely silent, because it does not print
goals any more."
  (or coq-run-completely-silent (coq--post-v92)))

(defun coq-configure-silent ()
  "Configure Proof General for running Coq silent or not.
See `coq--run-silent-p'.  This runs in `coq-pg-setup' and again in
`coq-shell-mode-config', because the version of Coq is not necessarily
known when the Coq file is opened."
  (when (and (not coq-run-completely-silent)
             (coq--post-v92)
             (not coq--run-silent-warned))
    (setq coq--run-silent-warned t)
    (display-warning
     'proof-general
     "`coq-run-completely-silent' is ignored for Rocq >= 9.2, which always runs completely silent."))
  (if (coq--run-silent-p)
      (setq
       ;; When proof-shell-start-silent-cmd and proof-shell-stop-silent-cmd
       ;; are nil, the generic code won't switch Coq to silent and noisy at,
       ;; respectively, the beginning and end of longer asserted regions.
       proof-shell-start-silent-cmd nil
       proof-shell-stop-silent-cmd nil
       ;; prooftree is only supported for Coq/Rocq running silent
       proof-tree-configured (coq--post-v810)
       proof-tree-get-proof-info 'coq-proof-tree-get-proof-info
       proof-tree-find-begin-of-unfinished-proof
         'coq-find-begin-of-unfinished-proof
       proof-tree-assistant-specific-urgent-action
         'proof-tree-coq-urgent-action)
    (setq proof-shell-start-silent-cmd "Set Silent."
          proof-shell-stop-silent-cmd "Unset Silent."
          proof-tree-configured nil)))

(defun coq-pg-set-backend-functions ()
  "Set the functions of the Proof General backend in the current buffer.
In the script, goals and response buffers: the commands common to all
backends call them."
  (setq coq-query-function #'coq-pg-query)
  (setq coq-query-result-function #'coq-pg-query-result)
  (setq coq-show-at-point-function #'coq-pg-show-at-point)
  (setq coq-set-flag-function #'coq-pg-set-flag)
  (setq coq-processed-end-function #'proof-unprocessed-begin)
  (setq coq-ask-printing-width-function
        #'coq-ask-adapt-printing-width-and-show))

;;;###autoload
(defun coq-pg-setup ()
  (set-keymap-parent (current-local-map) coq-pg-mode-map)
  (setq coq-terminator-insert-function #'coq-pg-terminator-insert)
  (coq-pg-set-backend-functions)
  
  ;; coq-mode colorize errors better than the generic mechanism
  (setq proof-script-color-error-messages nil)
  (setq proof-terminal-string ".")
  ;; superseded by coq-script-parse-function
  ;;(setq proof-script-command-end-regexp coq-script-command-end-regexp)
  (setq proof-script-parse-function #'coq-script-parse-function)
  (setq proof-script-comment-start comment-start)
  (setq proof-script-comment-end comment-end)
  (setq proof-script-insert-newlines nil)
  (setq proof-unnamed-theorem-name "Unnamed_thm") ; Coq's default name

  (setq proof-assistant-home-page coq-www-home-page)

  (setq proof-prog-name coq-prog-name)
  (setq proof-prog-name-guess t)

  ;; We manage file saveing via coq-compile-auto-save and for coq
  ;; it is not necessary to save files when starting a new buffer.
  (setq proof-query-file-save-when-activating-scripting nil)

  ;; Commands sent to proof engine
  (setq proof-showproof-command "Show."
        proof-context-command "Print All."
        proof-goal-command "Goal %s."
        proof-save-command "Save %s."
        proof-find-theorems-command "Search %s.")

  (setq proof-show-proof-diffs-regexp coq-show-proof-diffs-regexp)

  (setq proof-shell-empty-action-list-command 'coq-empty-action-list-command)

;; FIXME da: Does Coq have a help or about command?
;;	proof-info-command "Help"

  ;; Settings not defined with defpacustom because they have an unsupported
  ;; type.
  (setq proof-assistant-additional-settings
        '(coq-compile-quick coq-compile-keep-going
          coq-compile-auto-save coq-lock-ancestors coq-diffs))

  (setq proof-goal-command-p #'coq-goal-command-p
        proof-shell-last-cmd-left-goals-p #'coq-open-goals-p
        proof-find-and-forget-fn #'coq-find-and-forget
        pg-topterm-goalhyplit-fn #'coq-goal-hyp
        proof-state-preserving-p #'coq-state-preserving-p)

  (setq proof-query-identifier-command "Check %s.")
  ;;TODO: from v8.5 this wold be better:
  ;;(setq proof-query-identifier-command "About %s.")

  (setq proof-save-command-regexp coq-save-command-regexp
        proof-really-save-command-p #'coq-save-command-p ;pierre:deals with Proof <term>.
	proof-save-with-hole-regexp coq-save-with-hole-regexp
	proof-goal-with-hole-regexp coq-goal-with-hole-regexp
	proof-nested-undo-regexp coq-state-changing-commands-regexp
        proof-script-imenu-generic-expression coq-generic-expression)

  ;; span menu
  (setq proof-script-span-context-menu-extensions #'coq-create-span-menu)

  (coq-configure-silent)

  ;; proof-omit-proofs config
  (setq
   proof-omit-proofs-configured t
   proof-script-proof-start-regexp coq-proof-start-regexp
   proof-script-proof-end-regexp coq-proof-end-regexp
   proof-script-definition-end-regexp coq-definition-end-regexp
   proof-script-proof-admit-command coq-omit-proof-admit-command
   proof-script-cmd-prevents-proof-omission #'coq-cmd-prevents-proof-omission
   proof-script-cmd-force-next-proof-kept coq-cmd-force-next-proof-kept
   proof-omit-cheating-regexp coq-omit-cheating-regexp)

  ;; proof-check-report/proof-check-annotate config
  (setq
   proof-get-proof-info-fn #'coq-get-proof-info-fn
   proof-retract-command-fn #'coq-retract-command)


  (setq proof-cannot-reopen-processed-files nil)

  (proof-config-done)
  )

(defun coq-shell-mode-config ()
  "Initialization of `coq-shell-mode' that runs in the `*coq*' buffer.
The interaction buffer with Coq, `*coq*', uses a major mode that
is derived via `proof-shell-mode' from `scomint-mode'.  This
function runs as the body of `coq-shell-mode' when the `*coq*'
buffer is initialized, which happens when the Coq background
process is started.  This function intitalizes the Coq
personalization of Proof General in the interaction buffer with
Coq.  At the end, this function calls `proof-shell-config-done',
which initializes the Coq session, e.g., by sending
`proof-shell-init-cmd', respectively, `coq-shell-init-cmd' to Coq.

In normal interaction, the proof assistant is started because the
user assert some commands.  Therefore this function runs only
shortly before `proof-assert-command-hook', respectively,
`coq-adapt-printing-width'."
  (setq
   proof-shell-cd-cmd coq-shell-cd
   proof-shell-filename-escapes '(("\\\\" . "\\\\") ("\""   . "\\\""))
   proof-shell-clear-goals-regexp coq-shell-proof-completed-regexp
   proof-shell-proof-completed-regexp coq-shell-proof-completed-regexp
   proof-shell-error-regexp coq-error-regexp
   proof-shell-no-error-regexp coq-no-error-regexp
   proof-shell-interrupt-regexp coq-interrupt-regexp
   proof-shell-assumption-regexp coq-id
   proof-shell-theorem-dependency-list-regexp coq-shell-theorem-dependency-list-regexp
   ;; CRs now can and should be preserved (to support String-related theories),
   ;; see also this GitHub issue: https://github.com/ProofGeneral/PG/issues/773
   proof-shell-strip-crs-from-input nil

   pg-subterm-first-special-char ?\360
   ;; The next three represent path annotation information
   pg-subterm-start-char ?\372          ; not done
   pg-subterm-sep-char ?\373            ; not done
   pg-subterm-end-char ?\374            ; not done
   pg-topterm-regexp "\375"

   ;; FIXME: ideally, the eager annotation should just be a single "special" char,
   ;; this requires changes in Coq.
   proof-shell-eager-annotation-start coq-shell-eager-annotation-start
   proof-shell-eager-annotation-start-length 32
   ;; no warning face for the whole message, rely on font-lock and on
   ;; `coq-insert-tagged-text' instead
   proof-shell-eager-annotation-face nil

   ;; we need these two strings to be recognized so that the first appearing is
   ;; analyzed
   proof-shell-interactive-prompt-regexp "TcDebug ([0-9]+) >\\|Going to execute:"

   ;; ****** is added at the end of warnings in emacs mode, this is temporary I
   ;;        want xml like tags, and I want them removed before warning display.
   ;; I want the same for errors -> pgip

   proof-shell-eager-annotation-end coq-shell-eager-annotation-end ; done
   proof-shell-annotated-prompt-regexp coq-shell-prompt-pattern
   proof-shell-result-start "\372 Pbp result \373"
   proof-shell-result-end "\372 End Pbp result \373"

;   proof-shell-start-goals-regexp          "^\\(?:(dependent evars:[^)]*)\\s-+\\)?[0-9]+\\(?: focused\\)? subgoals?"
   proof-shell-start-goals-regexp          coq-start-goals-regexp
   proof-shell-end-goals-regexp
   (if coq-hide-additional-subgoals
       (setq proof-shell-end-goals-regexp coq-end-goals-regexp-hide-subgoals)
     (setq proof-shell-end-goals-regexp coq-end-goals-regexp-show-subgoals))

   proof-shell-init-cmd (coq-shell-init-cmd)

   proof-no-fully-processed-buffer t

   proof-dependency-menu-system-specific coq-dependency-menu-system-specific
   proof-dependencies-system-specific coq-dependencies-system-specific
   ;; Coq has no global settings?
   ;; (proof-assistant-settings-cmd)

   proof-shell-restart-cmd coq-shell-restart-cmd
   pg-subterm-anns-use-stack t)

  (setq proof-shell-font-lock-keywords 'coq-font-lock-keywords-1)

  ;; prooftree config
  (setq
   proof-tree-ignored-commands-regexp coq-proof-tree-ignored-commands-regexp
   proof-tree-navigation-command-regexp coq-navigation-command-regexp
   proof-tree-cheating-regexp coq-proof-tree-cheating-regexp
   proof-tree-current-goal-regexp coq-proof-tree-current-goal-regexp
   proof-tree-update-goal-regexp coq-proof-tree-update-goal-regexp
   proof-tree-existentials-state-start-regexp
                      coq-proof-tree-existentials-state-start-regexp
   proof-tree-existentials-state-end-regexp
                        coq-proof-tree-existentials-state-end-regexp
   proof-tree-additional-subgoal-ID-regexp
                              coq-proof-tree-additional-subgoal-ID-regexp
   proof-tree-branch-finished-regexp coq-proof-tree-branch-finished-regexp
   proof-tree-show-sequent-command 'coq-show-sequent-command
   proof-tree-find-undo-position 'coq-proof-tree-find-undo-position
   proof-tree-display-stop-command 'coq-proof-tree-disable-evars
   )

  ;; `coq-pg-setup' did this already when the Coq file was opened, but
  ;; the version of Coq may not have been known at that time (e.g.,
  ;; opam switch selected later with `opam-switch-set-switch').  It is
  ;; known now, because `coq-prog-args' has just detected it.
  (coq-configure-silent)

  (proof-shell-config-done))


(proof-eval-when-ready-for-assistant
    (easy-menu-define proof-goals-mode-aux-menu
      proof-goals-mode-map
      "Menu for Proof General goals buffer."
      (cons "Coq" coq-other-buffers-menu-entries)))

(proof-eval-when-ready-for-assistant
    (easy-menu-define proof-goals-mode-aux-menu
      proof-response-mode-map
      "Menu for Proof General response buffer."
      (cons "Coq" coq-other-buffers-menu-entries)))


(setq coq-goals-buffer-function (lambda () proof-goals-buffer))
(setq coq-response-buffer-function (lambda () proof-response-buffer))

(defun coq-goals-mode-config ()
  (coq-pg-set-backend-functions)
  (setq mode-line-process '(:eval (coq-goals-mode-line)))
  (setq pg-goals-change-goal "Show %s .")
  (setq pg-goals-error-regexp coq-error-regexp)
  (setq proof-goals-font-lock-keywords coq-goals-font-lock-keywords)
  (proof-goals-config-done))

(defun coq-response-config ()
  (coq-pg-set-backend-functions)
  (setq proof-response-font-lock-keywords coq-response-font-lock-keywords)
  ;; The line wrapping in this buffer just seems to make it less readable.
  (setq truncate-lines t)
  (proof-response-config-done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flags and other settings for Coq.
;; These appear on the Coq -> Setting menu.
;;

;; FIXME da: we should send this command only inside a proof,
;; otherwise it gives an error message.  It should be on
;; a different menu command.
;; (defpacustom print-only-first-subgoal  nil
;;  "Whether to just print the first subgoal in Coq."
;;  :type 'boolean
;;  :setting ("Focus." . "Unfocus."))


(defpacustom hide-additional-subgoals nil
  "Show all subgoals if off, show only the current goal if on."
  :type 'boolean
  :safe 'booleanp
  :eval (coq-hide-additional-subgoals-switch))


;
;; FIXME: to handle "printing all" properly, we should change the state
;; of the variables that also depend on it.
;; da:

;; pc: removed it and others of the same kind. Put an "option" menu instead,
;; with no state variable. To have the state we should use coq command that
;; output the value of the variables.
;(defpacustom print-fully-explicit nil
;  "Print fully explicit terms."
;  :type 'boolean
;  :setting ("Set Printing All." . "Unset Printing All."))
;

(defpacustom printing-depth 50
  "Depth of pretty printer formatting, beyond which dots are displayed."
  :type 'integer
  :setting "Set Printing Depth %i .")

(defun coq-diffs ()
  "Return string for setting Coq Diffs.
Return the empty string if the version of Coq < 8.10."
  (setq pg-insert-text-function #'coq-insert-tagged-text)
  (if (coq--post-v810)
      (format "Set Diffs \"%s\"." (symbol-name coq-diffs))
    ""))

(defun coq-diffs--setter (symbol new-value)
  ":set function to `coq-diffs'.
Set Diffs setting if Coq is running and has a version >= 8.10."
  (set symbol new-value)
  (if (proof-shell-available-p)
      (let ((cmd (coq-diffs)))
        (if (equal cmd "")
            (message "Ignore coq-diffs setting %s for Coq before 8.10"
                 (symbol-name coq-diffs))
          (proof-shell-invisible-command cmd)))))

(defcustom coq-diffs 'off
  "Controls Coq Diffs option."
  :type '(radio
    (const :tag "Don't show diffs" off)
    (const :tag "Show diffs: only added" on)
    (const :tag "Show diffs: added and removed" removed))
  :safe (lambda (v) (member v '(off on removed)))
  :set #'coq-diffs--setter
  :group 'coq)

;; Obsolete:
;;(defpacustom undo-depth coq-default-undo-limit
;;  "Depth of undo history.  Undo behaviour will break beyond this size."
;;  :type 'integer
;;  :setting "Set Undo %i .")

;; Problem if the Remove or Add fails we leave Coq's blacklist in a strange
;; state: unnoticed by the user, and desynched from
;; coq-search-blacklist-current-string.
;; TODO: have a way to detect error and re-set old value.
(defun coq-change-search-blacklist (s)
  (let* ((cmd-clean (list (format "Remove Search Blacklist %s."
                                  coq-search-blacklist-current-string)))
         (item-clean `(nil ,cmd-clean proof-done-invisible 'invisible))
         (cmd-set (list (format "Add Search Blacklist %s." s)))
         (item-set `(nil ,cmd-set proof-done-invisible 'invisible)))
  (proof-add-to-queue `(,item-clean ,item-set) 'advancing)))

(defun coq-change-search-blacklist-interactive ()
  (interactive)
  (proof-shell-ready-prover)
  (let* ((current-value coq-search-blacklist-current-string)
         (s (read-string
             "Set search blacklist to: "
             current-value 'proof-minibuffer-history)))
    (coq-change-search-blacklist s)
    (setq coq-search-blacklist-current-string s)))

;; (defpacustom search-blacklist coq-search-blacklist-string
;;   "Strings to blacklist in requests to Coq environment."
;;   :type 'string
;;   :get 'coq-get-search-blacklist
;;   :setting coq-set-search-blacklist)


(defpacustom time-commands nil
  "Whether to display timing information for each command."
  :type 'boolean
  :eval (coq-time-commands-switch))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; prooftree support
;;

(defun coq-proof-tree-get-proof-info ()
  "Coq instance of `proof-tree-get-proof-info'."
  (let* ((info (or (coq-last-prompt-info-safe) '(0 0 nil nil))))
         ;; info is now a list with
         ;; * the state number
         ;; * the proof stack depth
         ;; * the list of all open proofs
         ;; * the name of the current proof or nil
    (list (car info) (nth 3 info))))

(defun coq-show-sequent-command (sequent-id state)
  "Coq specific function for `proof-tree-show-sequent-command'."
  (format "Show Goal %s at %d." sequent-id state))


(defun coq-find-begin-of-unfinished-proof ()
  "Return start position of current unfinished proof or nil."
  (let ((span (span-at (1- (proof-unprocessed-begin)) 'type)))
    ;; go backward as long as we are inside the proof
    ;; the proofstack property is set inside the proof
    ;; the command before the proof has the goalcmd property
    (while (and span
                (span-property span 'proofstack)
                (not (span-property span 'goalcmd)))
          (setq span (span-at (1- (span-start span)) 'type)))
    ;; Beware of completed proofs! They have type goalsave and for
    ;; strange reasons the whole completed proof has the goalcmd property.
    (if (and span
             (not (eq 'goalsave (span-property span 'type)))
             (span-property span 'goalcmd))
        (span-start span)
      nil)))

(defun coq-proof-tree-find-undo-position (state)
  "Return the position for undo state STATE.
This is the Coq incarnation of `proof-tree-find-undo-position'."
  (let ((span-res nil)
        (span-cur (span-at (1- (proof-unprocessed-begin)) 'type))
        (state (1- state)))
    ;; go backward as long as the statenum property in the span is greater or
    ;; equal than state
    (while (<= state (span-property span-cur 'statenum))
      (setq span-res span-cur)
      (setq span-cur (span-at (1- (span-start span-cur)) 'type)))
    (span-start span-res)))


;; Since Coq 8.6 the evar line is disabled by default because on some
;; proofs it causes a severe performance hit. For its features on evar
;; creation and instantiation Prooftree needs the evar line and displays a
;; warning for parsing errors caused by the missing evar line. Proof
;; General therefore turns the evar line on when the proof tree display is
;; enabled and off when the proof is finished or the display is disabled.
;; This unconditional evar line switching is controled by
;; `coq-proof-tree-manage-dependent-evar-line'. However, the flag in Coq
;; that controls the printing of the dependent evar line will be restored
;; to its old state by undo. Therefore, a combination of proof tree display
;; and undo can cause the evar line to stay on until the next proof tree
;; display is finished. One such way is to disable the proof tree display
;; in the middle of a proof and then to undo a few tactics.

(defun coq-proof-tree-evar-command (cmd)
  "Return the evar printing command for CMD as action list item.
Adds, among others, `'empty-action-list' to flags to avoid that
`coq-empty-action-list-command' adds a show command because it
recognizes a change of a printing option.  When the user quits the
proof tree display inside prooftree, then the evar command likely
runs as single command."
  (proof-shell-action-list-item
   (concat cmd " Printing Dependent Evars Line.")
   'proof-done-invisible
   (list 'invisible 'no-response-display 'empty-action-list)))

(defun coq-proof-tree-enable-evars ()
  "Enable the evar status line."
  ;; The evar status can be switched on and off since 8.6. However, prooftree
  ;; is only supported for 8.10 or later.
  (when (and (coq--post-v86) coq-proof-tree-manage-dependent-evar-line)
    (proof-shell-ready-prover)
    (proof-add-to-priority-queue (coq-proof-tree-evar-command "Set"))))

(add-hook 'proof-tree-start-display-hook #'coq-proof-tree-enable-evars)

(defun coq-proof-tree-disable-evars ()
  "Return action list item to disable evar printing.
Function for `proof-tree-display-stop-command'."
  ;; The evar status can be switched on and off since 8.6. However, prooftree
  ;; is only supported for 8.10 or later.
  (when (and (coq--post-v86) coq-proof-tree-manage-dependent-evar-line)
    (coq-proof-tree-evar-command "Unset")))

(defun proof-tree-coq-urgent-action (last-item)
  "Insert show-goal commands when running completely silent.
Coq specific function for `proof-tree-assistant-specific-urgent-action'.
When running completely silent, insert a show-goal command for commands
coming from the user.  Be careful to not insert show-goal commands for
show-goal requests from prooftree or for the items inserted by this
function.  LAST-ITEM is the last proof-action item that has just been
processed.  This function is guaranteed to be called at a place where
`proof-action-list' can be directly manipulated.

When single stepping through a proof, this function inserts a second
show command, which is inserted before the one from
`coq-show-goals-inside-proof'.  One would of course be enough, but fixing
this would be difficult."
  (let ((flags (nth 3 last-item)))
    (unless (or (memq 'invisible flags)
                (memq 'proof-tree-show-subgoal flags))
      (let ((proof-info (coq-proof-tree-get-proof-info)))
        ;; Ignore retractions, prooftree only needs fresh sequents.
        (when (and (>= (car proof-info) proof-tree-last-state)
                   (cadr proof-info))
          (push
           (proof-shell-action-list-item
	    "Show."
            (lambda (_span)
              (proof-tree-display-goal-callback last-item proof-info))
	    '(invisible no-goals-display no-response-display
                        proof-tree-show-subgoal))
           proof-action-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; proof-check-report/proof-check-annotate support
;;

(defun coq-get-proof-info-fn ()
  "Coq instance of `proof-get-proof-info-fn' for `proof-check-proofs'.
Return state number followed by the name of the current proof of
nil in a list."
  (list
   coq-last-but-one-statenum
   (car coq-last-but-one-proofstack)))

(defun coq-retract-command (state)
  "Coq instance of `proof-retract-command-fn' for `proof-check-proofs'.
Return command that undos to state."
  (format "BackTo %d." state))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pre-processing of input string
;;


(defconst coq--time-prefix "Time "
  "Coq command prefix for displaying timing information.")

(defun coq-bullet-p (s)
  (string-match coq-bullet-regexp-nospace s))

;; Remark: `action' and `string' are known by `proof-shell-insert-hook'
(defun coq-preprocessing ()
  (when coq-time-commands
    (with-no-warnings  ;; NB: dynamic scoping of `string' and `action'
      ;; Don't add the prefix if this is a command sent internally
      (unless (or (eq action 'proof-done-invisible)
                  (coq-bullet-p string)) ;; coq does not accept "Time -".
        (setq string (concat coq--time-prefix string))
        ;; coqtop *really wants* a newline after a comand-ending dot.
        ;; (error) locations are very wrong otherwise
        (setq string (proof-strip-whitespace-at-end string))))))

(add-hook 'proof-shell-insert-hook #'coq-preprocessing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Subterm markup -- it was added to Coq by Healf, but got removed.
;;                   Let's try faking something by regexp matching.

;; FIXME: not operational yet
(defun coq-fake-constant-markup ()
  "Markup constants in Coq goal output by matching on regexps.
This is a horrible and approximate way of doing subterm markup.
\(Code used to be in Coq, but got lost between versions 5 and 7).
This is a hook setting for `pg-after-fontify-output-hook' to
enable identifiers to be highlighted and allow useful
mouse activation."
  (goto-char (point-min))
  ;; FIXME: This regexp is obviously wrong.  The backslashes have no effect
  ;; (for lack of being doubled), so Emacs searches for "(w+[^w])" :-(
  ;; Also the \w regexp operator cannot be used within a [...] char range.
  (while (re-search-forward "\(\w+[^\w]\)" nil t)
    (replace-match "\372\200\373\\1\374" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Context-senstive in-span menu additions
;;

(defun coq-create-span-menu (span idiom _name)
  (if (eq idiom 'proof)
      (let ((thm (span-property span 'name)))
        (list (vector
               "Check" ; useful?
               `(proof-shell-invisible-command
                 ,(format "Check %s." thm)))
              (vector
               "Print"
               `(proof-shell-invisible-command
                 ,(format "Print %s." thm)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some smart insertion functions
;;

;; The insertion of "as" clauses is in coq-queries.el.

;::::::::::::: inserting suggested Proof using XXX... ;;;;;;;;;;
;; global mechanism:

;; When some dependency oinformation is detected (see
;; `coq-shell-theorem-dependency-list-regexp') a hook is triggered by pg. We
;; configure this hook to perform the following: if the user set
;; `coq-accept-proof-using-suggestion' to 'always, then perform the insertion
;; immediately via using `coq-insert-proof-using', otherwise add the menu entry
;; to make it later (actually the current impolementation always adds the menu
;; entry, which is wrong because the span is not there if the insertion was
;; already performed.).


;; This variable is used by generic pg code. Every time this is detected in the
;; output, it sets the `proof-last-theorem-dependencies' variable. Substring 1
;; should contain the name of the theorem, and substring 2 should contain its
;; dependencies. The content of `proof-last-theorem-dependencies' is then used
;; by pg generic code to trigger `proof-depends-process-dependencies', which
;; itself sets the 'dependencies property of the span, and calls
;; `proof-dependencies-system-specific'. The latter is bound to
;; `coq-dependencies-system-specific' below.
(defconst coq-shell-theorem-dependency-list-regexp
  "<infomsg>\n?The proof of \\(?1:[^ \n]+\\)\\(?: \\|\n\\)should start with one of the following commands:\\(?: \\|\n\\)Proof using\\(?2:[^.]*\\)\\.")

(defcustom coq-accept-proof-using-suggestion 'highlight
  "Whether and how ProofGeneral should insert \"Proof using\" suggestions.
Suggestions are emitted by Coq at Qed time.  The possible values
of this variable are:

- \\+`always': always insert the suggested annotation

- \\+`highlight' (default value): highlight the Proof command and
  have a contextual menu for insertion (or M-x
  coq-insert-suggested-dependency when point is on the proof
  considered)

- \\+`ask': asks the user each time.  If yes then do as \\+`always', else
  do as \\+`highlight'

- \\+`never': ignore completely the suggestions.

Remarks and limitations:
- do not support nested proofs.
- always use the *first* annotation suggested by Coq.
- the proof is not replayed, which is consistent with the fact
  that pg currently do not deal with async proofs.
- if there is already a \"Proof using\" annotation, then either it
  is correct and nothing happens, or it is incorrect and Coq
  generates an error. PG will not try to replace the erroneous
  annotation in this case, but you can always go back, remove it
  by hand, and let pg insert the good one.
- instead of the menu you can do \\[coq-insert-suggested-dependency]
  when point is on the proof considered."
  :type '(choice
	  (const :tag "Ask user" ask)
	  (const :tag "Always accept" always)
	  (const :tag "Higihlight" never)
	  (const :tag "Ignore completely" ignore))
  :group 'coq)

;; putting "Type" instead of nothing, otherwise Coq may fail if a "with"
;; anotation is also present: (Proof using with auto raises an error in
;; coq<=8.12 at least).
(defun coq-hack-proofusing-suggestion (suggested)
  (if (string-equal "" suggested) "Type" suggested))

;; the additional menu for "proof using". highlights the "Proof." command, and
;; have a entry to insert the annotation and remove the highlight.
(defvar coq-dependency-menu-system-specific
  (lambda (span)
    (let* ((deps (span-property-safe span 'dependencies))
           (specialspans (spans-at-region-prop (span-start span) (span-end span) 'proofusing))
           (specialspan (and specialspans (not (cdr specialspans)) (car specialspans)))
           (suggested (mapconcat #'identity deps " "))
           (suggested (coq-hack-proofusing-suggestion suggested))
           (name (concat " insert \"proof using " suggested "\""))
           (fn (lambda (sp)
                 (coq-insert-proof-using-suggestion sp t)
                 (and specialspan (span-delete specialspan)))))
      (list "-------------" (vector name `(,fn ,span) t))))
  "Coq specific additional menu entry for \"Proof using\".
annotation. See `proof-dependency-menu-system-specific'." )

(defconst coq-proof-using-regexp "\\_<Proof\\(?1:[^.]*\\)\\."
  "Regexp matching Coq \"Proof ....\" annotation (with no \"using\" annotation).
We suppose there is no \"using\" annotation, since Coq will fail
in this case and no suggestion can be added without replaying the
script.  Actually the only possible content after Proof is a
\"with annotation\".
The substring matched numbered 1 must start at the possible
insertion point for the \"using\" annotation.")


;; span is typically the whole theorem statement+proof span built after a save
;; command
(defun coq-highlight-span-dependencies (start end _suggested)
  (goto-char start)
  ; Search for the "Proof" command and build a hilighted span on it
  (let* ((proof-pos (match-beginning 0))
         (newspan (span-make start end)))
    (span-set-property newspan 'face 'proof-warning-face)
    (span-set-property newspan 'help-echo "Right click to insert \"proof using\"")
    (span-set-property newspan 'proofusing t)))

(defun coq-insert-proof-using (_proof-pos _previous-content insert-point string-suggested)
  (goto-char insert-point)
  (let ((proof--inhibit-retract-on-change t))
    (insert (concat " using " string-suggested))))

(defun coq-insert-suggested-dependency ()
  (interactive)
  (let* ((span (span-at (point) 'type))
         ;; (deps (span-property-safe span 'dependencies))
         (specialspans (spans-at-region-prop (span-start span) (span-end span) 'proofusing))
         (specialspan (and specialspans (not (cdr specialspans)) (car specialspans))))
    (coq-insert-proof-using-suggestion span t)
    (span-delete specialspan)))

(defun coq-find-Proof-command (start end)
  "Look for a \"Proof\" command in span SPAN.

Return nil if not found.  Set point to the end of \"Proof\"
otherwise and return position."
  (goto-char start)
  (let ((res t) found)
    (while (and res (null found))
      (setq res (let ((case-fold-search nil))
                  (re-search-forward coq-proof-using-regexp end t)))
      (when res
        (let ((beg-proof (match-beginning 0))
              (ins-point (match-beginning 1))
              (end-cmd (match-end 1)))
          (if (and res
                   (save-excursion ;; one more check that it is a Proof command
                     (goto-char beg-proof)
                     (equal (point) (coq-find-real-start))))
              (setq found (list beg-proof ins-point end-cmd))
            (goto-char end-cmd)))))
    found))

;; TODO: have 'ignoe option to completely ignore (not highlight)
;; and have 'never renamed into 'highlight
(defun coq-insert-proof-using-suggestion (span &optional force)
  "Add the proof using annotation, respecting `coq-accept-proof-using-suggestion'.
Insert \" using xxx\" After the \"Proof\" of SPAN, where xxx is
built from the list of strings in SUGGESTED.
SPAN is the span of the whole theorem (statement + proof)."
  (with-current-buffer proof-script-buffer
    (save-excursion
      (let ((lproof-info (coq-find-Proof-command (span-start span) (span-end span))))
        (when lproof-info
          (let* ((proof-pos (car lproof-info)) ;(proof-pos (match-beginning 0))
                 (insert-point (cadr lproof-info)) ;(insert-point (match-beginning 1))
                 (proof-end (cl-caddr lproof-info))
                 (previous-string (buffer-substring insert-point proof-end))
                 (previous-content (split-string previous-string))
                 (suggested (span-property span 'dependencies))
                 (string-suggested (mapconcat #'identity suggested " "))
                 (string-suggested (coq-hack-proofusing-suggestion string-suggested))
                 ;; disabled for now it never happens because Coq wouldn't suggest anything
                 (same (and nil previous-content
                            (not (cl-set-exclusive-or previous-content suggested
                                                      :test 'string-equal))))
                 (usersayyes
                  (and (not same) (not force)
                       (equal coq-accept-proof-using-suggestion 'ask)
                       (y-or-n-p (concat "Update proof using annotation ("
                                         string-suggested ")")))))
            (unless same
              (if (or force (equal coq-accept-proof-using-suggestion 'always) usersayyes)
                  (coq-insert-proof-using proof-pos previous-content insert-point string-suggested)
                (when (member coq-accept-proof-using-suggestion '(highlight ask))
                  (coq-highlight-span-dependencies proof-pos proof-end string-suggested)
                  (message "\"Proof using\" not set. M-x coq-insert-suggested-dependency or right click to add it. See also `coq-accept-proof-using-suggestion'."))))))))))

(defvar coq-dependencies-system-specific
  (lambda (span)
    (coq-insert-proof-using-suggestion span))
  "Coq specific dependency mechanism.
Used for automatic insertion of \"Proof using\" annotations.")


;; Trying to propose insertion of "as" for a whole region. But iterating
;; proof-assert-next-command-interactive is probably wrong if some error occur
;; during scripting.
;; (defun coq-insert-as-in-region (&optional beg end)
;;   (interactive "r")
;;   (let ((beg (or beg (point-min)))
;;         (end (or end (point-max))))
;;     (goto-char beg)
;;     (while (< (point) end)
;;       (coq-script-parse-cmdend-forward)
;;       (coq-insert-as-in-next-command)
;;       (proof-assert-next-command-interactive))))



; Query commands

;(proof-eval-when-ready-for-assistant
; (define-key ??? [(control c) (control a)] (proof-ass keymap)))

;(proof-eval-when-ready-for-assistant
; (define-key ??? [(control c) (control a)] (proof-ass keymap)))

;; The keys of the queries and of the display, as in the script buffer.
(coq-queries-define-keys coq-goals-mode-map)
;; specific to goals buffer: (un)foldinng and (un)highlighting shortcuts
(coq-hyps-define-keys coq-goals-mode-map)

(coq-queries-define-keys coq-response-mode-map)
;; Before C-c C-a r and g, kept for compatibility.
(define-key coq-response-mode-map (kbd "C-c C-a C-r") #'coq-store-response-win)
(define-key coq-response-mode-map (kbd "C-c C-a C-g") #'coq-store-goals-win)

;; The queries at mouse click, see `coq-remap-mouse-1'.
(coq-mouse-query-add-keymap coq-response-mode-map)
(coq-mouse-query-add-keymap coq-goals-mode-map)



;;;;;;;;;;;;;;;;;;;;;;;;
;; error handling
;;;;;;;;;;;;;;;;;;;;;;;;


(defvar last-coq-error-location nil
  "Last error from `coq-get-last-error-location' and `coq-highlight-error'.")

(defvar coq--error-location-regexp
  "^Toplevel input, characters \\(?1:[0-9]+\\)-\\(?2:[0-9]+\\):\\(\n> [^\n]*\\)*\n"
  "A regexp to search the header of Coq error locations.")

;; I don't use proof-shell-last-output here since it is not always set to the
;; really last output (specially when a *tactic* gives an error) instead I go
;; directly to the response buffer. This allows also to clean the response
;; buffer (better to only scroll it?)
(defun coq-get-last-error-location (&optional parseresp clean)
  "Return location information on last error sent by Coq.

Return a two elements list (POS LEN) if successful, nil otherwise.

POS is the number of **bytes** preceding the error location in
the current command, and LEN is its length (in bytes too).

Coq error message is like this:

\"
Toplevel input, characters 22-26:
> command with an error here ...
>                       ^^^^
\"

If PARSERESP is nil, don't really parse response buffer but take the value of
`last-coq-error-location' instead, otherwise parse response buffer and updates
`last-coq-error-location'.

If PARSERESP and CLEAN are non-nil, delete the error location from the response
buffer."
  (if (not parseresp) last-coq-error-location
    (proof-with-current-buffer-if-exists proof-response-buffer
      (goto-char (point-max))
      (if (not (re-search-backward coq--error-location-regexp nil t))
          (setq last-coq-error-location (list 0 0))
        (let ((pos (string-to-number (match-string 1)))
              (end (string-to-number (match-string 2))))
          (setq last-coq-error-location (list pos (- end pos)))
          ;; clean the response buffer from ultra-ugly underlined command line
          ;; Don't kill the first \n
          ;; TODO: make coq stop displaying it it?
          (let ((inhibit-read-only t))
            (when clean (delete-region (match-beginning 0) (match-end 0)))))
        last-coq-error-location))))

(defun point-add-bytes (nbytes)
  "Return current point shifted by NBYTES bytes."
  (byte-to-position (+ (position-bytes (point)) nbytes)))

(defun coq-highlight-error (&optional parseresp clean)
  "Return position and length of error in last message.

Highlight the text pointed by it.
Coq error message must be like this:

\"
Toplevel input, characters 60-66:
> ...
>                       ^^^^
\"

If PARSERESP is nil, don't really parse response buffer but take the value of
`last-coq-error-location' instead, otherwise parse response buffer and updates
`last-coq-error-location'.

If PARSERESP and CLEAN are non-nil, delete the error location from the response
buffer.

Important: Coq gives char positions in bytes instead of chars."
  (proof-with-current-buffer-if-exists proof-script-buffer
    (let ((mtch (coq-get-last-error-location parseresp clean)))
      (when mtch
        (let ((pos (car mtch))
              (lgth (cadr mtch)))
          (goto-char (+ (proof-unprocessed-begin) 1))
          (coq-find-real-start)
          ;; locations are given in bytes. translate into valid positions
          ;; + deal with coq bug #19355
          (let* ((cmdstart (point))
                 (next-cmd (progn (coq-script-parse-cmdend-forward) (point)))
                 ;; test suspîcious location info from coq (coq#19355). coq <=
                 ;; 8.20 on curly braces gives the global location instead of
                 ;; the location inside the command. fallback: we locate the
                 ;; error at the command start, with given length. we detect
                 ;; the problem by testing if the location of the error is
                 ;; greater than the end of the command itself.
                 (bug19355 (> pos next-cmd)))
            (goto-char cmdstart)
              (let* ((pos (if bug19355 0 pos)) ;; 0 means start of command
                     (beg (goto-char (point-add-bytes pos)))
                     (end (goto-char (point-add-bytes lgth))))
                (span-make-self-removing-span beg end 'face 'proof-warning-face)
                (span-make-self-removing-span cmdstart next-cmd 'face 'proof-boring-face)
                ;; user usually expect the point to move to the error location
                (goto-char beg))))))))

(defun coq-highlight-error-hook ()
  (coq-highlight-error t t))

(add-hook 'proof-shell-handle-error-or-interrupt-hook #'coq-highlight-error-hook t)


(defun coq-show-goals-inside-proof (keep-response on-error)
  "Update goals when action item list has been processed, if running silent.
If Coq runs completely silent (see `coq--run-silent-p'), add a
Show command as priority action, such that it will still be
processed if the generic machinery inside `proof-shell-filter'
believes it has processed the last item from the action list.  When Coq runs in
silent mode, we need to update the goals precisely when
everything else from the action list has been processed.

The Show command is only added inside proofs and only if the last
processed command was not a show command from this function.  The
action item flag `'dont-show-when-silent' is used for the latter.

KEEP-RESPONSE should be set if the last command produced some error or
response that should be kept and shown to the user.  If set, the flag
`'keep-response' is added to the action item.  When the show command is
processed in `proof-shell-handle-delayed-output', the flag causes that
the response buffer is not cleared and that in 2-pane mode the goals are
not explicitly shown, see `pg-goals-display'.  ON-ERROR should be set
when there is some important message in the response buffer.  In this
case `'no-response-display' is added to the flags such that a \"no more
goals\" reply to the Show command does not overwrite the response
buffer.

Do nothing if Coq does not run completely silent."
  (when (and (coq--run-silent-p)
             coq-last-but-one-proofstack
             (not (member 'dont-show-when-silent
                          proof-shell-delayed-output-flags)))
    (let* ((flags-1 (list 'dont-show-when-silent 'invisible 'empty-action-list))
           (flags-2 (if on-error (cons 'no-response-display flags-1) flags-1))
           (flags (if keep-response (cons 'keep-response flags-2) flags-2)))
      (proof-add-to-priority-queue
       (proof-shell-action-list-item "Show." 'proof-done-invisible flags)))))

(defun coq-show-goals-on-error ()
  "Update goals after error.
This function is intended for
`proof-shell-handle-error-or-interrupt-hook' to update the goals
buffer after an error has been detected.  See also
`coq-show-goals-inside-proof'."
  (coq-show-goals-inside-proof t t))

(defun coq-show-goals-standard-case ()
  "Update goals after last command when no error was detected.
This function is intended for
`proof-shell-handle-delayed-output-hook' to update the goals
buffer after the last command has been processed and no error has
been detected.  Take care to keep the response buffer visible if
the last command was a Search, a Check or something similar.  See
also `coq-show-goals-inside-proof'."
  (coq-show-goals-inside-proof (eq proof-shell-last-output-kind 'response) nil))

(add-hook 'proof-shell-handle-error-or-interrupt-hook
          #'coq-show-goals-on-error)
(add-hook 'proof-shell-handle-delayed-output-hook
          #'coq-show-goals-standard-case)

;;
;; Scroll response buffer to maximize display of first goal
;;

(defun coq-show-first-goal ()
  "Scroll the goal buffer so that the first goal is visible.
First goal is displayed on the bottom of its window, maximizing the
number of hypothesis displayed, without hiding the goal.  See
`coq-goals-show-first-goal'."
  (interactive)
  ;; CPC 2015-12-31: Added the check below: if the command that caused this
  ;; call was silent, we shouldn't touch the goals buffer.  See GitHub issues
  ;; https://github.com/cpitclaudel/company-coq/issues/32 and
  ;; https://github.com/cpitclaudel/company-coq/issues/8.
  (unless (memq 'no-goals-display proof-shell-delayed-output-flags)
    (with-current-buffer proof-goals-buffer
      (coq-goals-fold-other-goals))
    (coq-goals-show-first-goal proof-goals-buffer)))

(defun coq-clean-goals-outside-proof ()
  "Clear goals buffer outside proofs.
This function ensures that the goals buffer is reset after Qed or
Admitted.  This function is for
`proof-shell-handle-delayed-output-hook'.

A proof completed message which has been displayed in the goals
buffer (see `coq-start-goals-regexp') is kept."
  (when (or (not coq-last-but-one-proofstack)
            (and (not (eq proof-shell-last-output-kind 'goals))
                 (proof-string-match coq-shell-proof-completed-regexp
                                     proof-shell-last-output)))
    (proof-clean-buffer proof-goals-buffer)))


;; This hook must be added before coq-optimise-resp-windows, in order to be evaluated
;; *after* windows resizing.
(add-hook 'proof-shell-handle-delayed-output-hook
	  #'coq-show-first-goal)
(add-hook 'proof-shell-handle-delayed-output-hook
          #'coq-clean-goals-outside-proof)

(defun coq-mark-hyps-in-goals ()
  "Mark the hypotheses of new goals for folding and highlighting.
See `coq-hyps-mark'.  The goals are new if not marked yet."
  (when (buffer-live-p proof-goals-buffer)
    (with-current-buffer proof-goals-buffer
      (unless (text-property-not-all (point-min) (point-max) 'coq-hyp nil)
        (pcase-dolist (`(,names ,start ,_name ,end-name ,_type ,end)
                       (save-excursion (coq-detect-hyps-positions-in-goals)))
          (coq-hyps-mark names start end-name end))
        (coq-hyps-decorate t)))))

(add-hook 'proof-shell-handle-delayed-output-hook #'coq-mark-hyps-in-goals)



(defun is-not-split-vertic (_selected-window)
  (<= (- (frame-height) (window-height)) 2))

;; bug fixed in generic ocde, useless now:
;(add-hook 'proof-activate-scripting-hook (lambda () (when proof-three-window-enable (proof-layout-windows))))

;; *Experimental* auto shrink response buffer in three windows mode. Things get
;; a bit messed up if the response buffer is not at the right place (below
;; goals buffer) TODO: Have this linked to proof-resize-window-tofit in
;; proof-utils.el + customized by the "shrink to fit" menu entry
;;  + have it on by default when in three windows mode.
;; The philosophy is that if goals and response are on the same column, their
;; cumulated size should not change.
(defun coq-optimise-resp-windows ()
  "Resize response buffer to optimal size.
Only when three-buffer-mode is enabled."
  ;; CPC 2015-12-31: Added the check below: if the command that caused this
  ;; call was silent, we shouldn't touch the response buffer.  See GitHub
  ;; issues https://github.com/cpitclaudel/company-coq/issues/32 and
  ;; https://github.com/cpitclaudel/company-coq/issues/8.
  (unless (memq 'no-response-display proof-shell-delayed-output-flags)
    ;; If there is no frame with goql+response then do nothing
    (when proof-three-window-enable
      (let ((threeb-frames (coq-find-threeb-frames)))
        (when threeb-frames
          (let ((pg-frame (car threeb-frames))) ; selecting one adequate frame
            (with-selected-frame pg-frame
              (let ((response-window (get-buffer-window proof-response-buffer t))
                    (goals-window (get-buffer-window proof-goals-buffer t)))
                (when (and response-window
                           (> (frame-height) 10))
                  (with-selected-window response-window
                    (with-current-buffer proof-response-buffer
                      (let* ((response-height (window-text-height response-window))
                             (goals-height (window-text-height goals-window))
                             (maxhgth (- (+ response-height goals-height)
                                         window-min-height))
                             (nline-resp ; number of lines we want for response buffer
                              (min maxhgth (max window-min-height ; + 1 for comfort
                                                (+ 1 (count-lines (point-max) (point-min)))))))
                        (unless (is-not-split-vertic (selected-window))
                          (shrink-window (- response-height nline-resp)))
                        (goto-char (point-min))
                        (recenter)))))))))))))

;; TODO: remove/add hook instead?
(defun coq-optimise-resp-windows-if-option ()
  (when coq-optimise-resp-windows-enable (coq-optimise-resp-windows)))

;; TODO: I would rather have a response-insert-hook thant this two hooks
;; Careful: coq-optimise-resp-windows must be called BEFORE proof-show-first-goal,
;; i.e. added in hook AFTER it.

;; Adapt when displaying a normal message
(add-hook 'proof-shell-handle-delayed-output-hook
          #'coq-optimise-resp-windows-if-option)
;; Adapt when displaying an error or interrupt
(add-hook 'proof-shell-handle-error-or-interrupt-hook
          #'coq-optimise-resp-windows-if-option)

;;; DOUBLE HIT ELECTRIC TERMINATOR
;; Double hit on "." behaves like the electric terminator, see
;; pg-double-hit.el.  The "." is used for records and modules qualified
;; notations, so the electric terminator is not pertinent.

(defun coq-pg-terminator-insert (&optional count)
  "A wrapper on `proof-electric-terminator', COUNT is the prefix argument.
Used to accept double hits instead if enabled, see
`proof-double-hit-enable'.  If by accident option
`proof-electric-terminator-enable' and option `proof-double-hit-enable'
are non-nil at the same time, this gives priority to the former.
This is `coq-terminator-insert-function' for Proof General."
  (if (and (not proof-electric-terminator-enable) proof-double-hit-enable)
      (proof-double-hit-insert #'proof-assert-electric-terminator count
                               proof-terminal-string)
    ;; otherwise call this, which checks proof-electric-terminator-enable
    (proof-electric-terminator count)))


;;;;;;;;;;;;;; kill coq background process on different occasions ;;;;;;;;;;;;;;

;; This originates from background compilation, but it is actually a good idea
;; even when "compile when require" is off. When switching scripting buffer,
;; let us restart the coq shell process, so that it applies local coqtop
;; options.

(defun coq-kill-proof-shell ()
  "Kill the proof shell without asking the user.
This function is for `proof-deactivate-scripting-hook'.  It kills
the proof shell without asking the user for
confirmation (assuming she agreed already on switching the active
scripting buffer).  This is needed to ensure the load path is
correct in the new scripting buffer."
  (unless proof-shell-exit-in-progress
    (proof-shell-exit t)))

(add-hook 'proof-deactivate-scripting-hook #'coq-kill-proof-shell t)

(defcustom coq-kill-coq-on-opam-switch t
  "If t kill Coq when the opam switch changes (requires `opam-switch-mode').
When `opam-switch-mode' is loaded and the user changes the opam switch
through `opam-switch-mode' then this option controls whether the Coq
background process (the proof shell) is killed such that the next
assert command starts a new proof shell, probably using a
different Coq version from a different opam switch.

See https://github.com/ProofGeneral/opam-switch-mode for `opam-switch-mode'"
  :type 'boolean
  :group 'coq)


(defun coq-kill-proof-shell-on-opam-switch ()
  "Kill proof shell when the opam switch changes (requires `opam-switch-mode').
This function is for the `opam-switch-mode' hook
`opam-switch-change-opam-switch-hook', which runs when the user
changes the opam switch through `opam-switch-mode'.  If
`coq-kill-coq-on-opam-switch' is t, then the proof shell is
killed such that the next assert starts a new proof shell, using
Coq from the new opam switch."
  (when (and coq-kill-coq-on-opam-switch proof-script-buffer)
    (coq-kill-proof-shell)))


(add-hook 'opam-switch-change-opam-switch-hook
          #'coq-kill-proof-shell-on-opam-switch t)

;;;;;;;;;;;;;;;

;; overwriting the default behavior, this is an experiment, *frames* will be
;; deleted only if only displaying associated buffers. If this is OK the
;; function itself will replace the other in generic.
(defun proof-delete-other-frames () (proof-delete-all-associated-windows))

(provide 'coq)

;;   Local Variables: ***
;;   fill-column: 79 ***
;;   indent-tabs-mode: nil ***
;;   coding: utf-8 ***
;;   End: ***

;;; coq.el ends here
