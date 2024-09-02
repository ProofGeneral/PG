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
(defvar coq-use-editing-holes)          ; defpacustom
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

(defcustom coq-user-init-cmd nil
  "User defined init commands for Coq.
These are appended at the end of `coq-shell-init-cmd'."
  :type '(repeat (cons (string :tag "command")))
  :group 'coq)

(defcustom coq-optimise-resp-windows-enable t
  "If non-nil (default) resize vertically response windw after each command."
  :type 'boolean
  :group 'coq)

;; Default coq is only Private_ and _subproof
(defcustom coq-search-blacklist-string ; add this? \"_ind\" \"_rect\" \"_rec\"
 "\"Private_\" \"_subproof\""
  "Initial strings to blacklist in requests to Coq environment.
If you are setting this via emacs cutomization menus, you should
restart Coq to see the effect. To change blacklist during a coq
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

(defcustom coq-prefer-top-of-conclusion nil
  "Prefer start of the conclusion over its end when displaying large goals.
Namely, goals that do not fit in the goals window."
  :type 'boolean
  :group 'coq)


(defconst coq-shell-init-cmd
  (append
   `(
     ;; Should this variable be buffer-local? No opinion on that but if yes we
     ;; should re-intialize to coq-search-blacklist-string instead of
     ;; keeping the current value (that may come from another file).
     ,(format "Add Search Blacklist %s." coq-search-blacklist-current-string))
   '("Set Suggest Proof Using.") coq-user-init-cmd)
  "Command to initialize the Coq Proof Assistant.")

;; FIXME: Even if we don't use coq-indent for indentation, we still need it for
;; coq-script-parse-cmdend-forward/backward and coq-find-real-start.
(require 'coq-indent)


;; Command to reset the Coq Proof Assistant
(defconst coq-shell-restart-cmd "Reset Initial.\n ")

(defvar coq-shell-prompt-pattern
  "\\(?:\n\\(?:[^\n\371]+\371\\|<prompt>[^\n]+</prompt>\\)\\)"
  "*The prompt pattern for the inferior shell running coq.")

;; FIXME da: this was disabled (set to nil) -- why?
;; da: 3.5: add experimental
;; am:answer: because of bad interaction
;; with coq -R option.
(defvar coq-shell-cd nil
;;  "Add LoadPath \"%s\"." ;; fixes unadorned Require (if .vo exists).
  "*Command of the inferior process to change the directory.")

(defvar coq-shell-proof-completed-regexp
  (concat "No\\s-+more\\s-+\\(?:sub\\)?goals\\.\\|Subtree\\s-proved!\\|"
          "Proof\\s-completed\\|"
          ;; if printing width is small, eg. when running in batch mode,
          ;; there might be a line break after infomsg
          "<infomsg>\n?.*\\s-is\\s-declared"
          ;; \\|This subproof is complete
          )
  "*Regular expression indicating that the proof has been completed.
Coq instance of `proof-shell-clear-goals-regexp'. Used in
`proof-shell-process-urgent-message' to determine if the goals
buffer shall be cleaned. Some of the messages recognized here are
not printed by Coq in silent mode, such that Proof General might
fail to delete the goals buffer.")

(defvar coq-goal-regexp
  "\\(============================\\)\\|\\(\\(?:sub\\)?goal [0-9]+\\)\n")


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
  "^\\(\\(Show\\)\\|\\(Locate\\)\\)"
  "Regexp for `proof-tree-ignored-commands-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-navigation-command-regexp
  (concat "^\\(\\(Focus\\)\\|\\(Unfocus\\)\\|"
          "\\(all\\s-*:\\s-*\\(cycle\\|swap\\|revgoals\\)\\)\\|"
          "\\(\\+\\)\\|\\(-\\)\\|\\(\\*\\)\\|\\({\\)\\|\\(}\\)\\)")
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


(defun coq-remove-starting-blanks (s)
  (string-match "\\`\\s-*" s)
  (substring s (match-end 0) (length s)))


;; Messages dispalyed by "Time" are always following or preceding the real
;; result, if it follows we want it to be non urgent, otherwise the result
;; would not be shown in response buffer. If it is before, then we want it
;; urgent so that it is displayed.
(defvar coq-eager-no-urgent-regex "\\s-*Finished "
  "Regexp of commands matching ‘proof-shell-eager-annotation-start’
that should maybe not be classified as urgent messages.")

;; return the end position if found, nil otherwise
(defun coq-non-urgent-eager-annotation ()
  (save-excursion
    (when (and (looking-at coq-eager-no-urgent-regex)
               (re-search-forward proof-shell-eager-annotation-end nil t))
      (let ((res (match-end 0))); robustify
        ;; if there is something else than a prompt here then this eager
        ;; annotation is left urgent (return nil), otherwise it is not urgent
        ;; (return position of the end of the annotation)
        (when (looking-at (concat "\\s-*" proof-shell-annotated-prompt-regexp))
          res)))))

;; Looking for eager annotation which does not match coq-eager-no-urgent-regex
(defun coq-search-urgent-message ()
  "Find forward the next really urgent message.
Return the position of the beginning of the message (after the
annotation-start) if found."
  (let ((again t) (start-start nil) (end-end nil) ;; (found nil)
        (eager proof-shell-eager-annotation-start))
    (while again
      (setq start-start (and (re-search-forward eager nil 'limit)
                             (match-beginning 0)))
      (setq end-end (and start-start (coq-non-urgent-eager-annotation)))
      (unless end-end
        (setq again nil) ; exit while
        ;; display message (this prints the message once now but it will be
        ;; reprinted as a normal output, bad?)
        ;;(proof-shell-process-urgent-message start-start end-end)
        ))
    (and start-start (goto-char start-start))))

;; This is a modified version of the same function in generic/proof-shell.el.
;; Using function coq-search-urgent-message instead of regex
;; proof-shell-eager-annotation-start, in order to let non urgent message as
;; such. i.e. "Time" messages.
(defun proof-shell-process-urgent-messages ()
  "Scan the shell buffer for urgent messages.
Scanning starts from `proof-shell-urgent-message-scanner' or
`scomint-last-input-end', which ever is later.  We deal with strings
between regexps `proof-shell-eager-annotation-start' and
`proof-shell-eager-annotation-end'.

We update `proof-shell-urgent-message-marker' to point to last message found.

This is a subroutine of `proof-shell-filter'."
  (let ((pt (point)) (end t)
	lastend laststart
	(initstart (max  (marker-position proof-shell-urgent-message-scanner)
			 (marker-position scomint-last-input-end))))
    (goto-char initstart)
    (while (and end (setq laststart (coq-search-urgent-message))
                )
;      (setq laststart (match-beginning 0))
      (if (setq end
		(re-search-forward proof-shell-eager-annotation-end nil t))
	  (save-excursion
	    (setq lastend end)
	    ;; Process the region including the annotations
	    (proof-shell-process-urgent-message laststart lastend))))

    (set-marker
     proof-shell-urgent-message-scanner
     (if end ;; couldn't find message start; move forward to avoid rescanning
         (max (or lastend 1) ;; goto after last end urgent msg
              ;; or near the end of current output if that jumps farther.
	      (- (point)
		 (1+ proof-shell-eager-annotation-start-length)))
       ;; incomplete message; leave marker at start of message
       laststart))

    ;; Set position of last urgent message found
    (if lastend
	(set-marker proof-shell-urgent-message-marker lastend))

    (goto-char pt)))

;; Due to the architecture of proofgeneral, informative message put *before*
;; the goal disappear unless marked as "urgent", i.e. being enclosed with
;; "eager-annotation" syntax. Since we don't want the Warning color to be used
;; for simple informative message, we have to redefine this function to use
;; normal face when the "eager annotation" is acutally not a warning. This is a
;; modified version of the same function in generic/proof-shell.el.
(defun proof-shell-process-urgent-message-default (start end)
  "A subroutine of `proof-shell-process-urgent-message'."
  ;; Clear the response buffer this time, but not next, leave window.
  (pg-response-maybe-erase nil nil)
  (proof-minibuffer-message
   (buffer-substring-no-properties
    (save-excursion
      (re-search-forward proof-shell-eager-annotation-start end nil)
      (point))
    (min end
         (save-excursion (end-of-line) (point))
         (+ start 75))))
  (goto-char start)
  (let* (;; (face (if (looking-at "<infomsg>") 'default
         ;;         'proof-eager-annotation-face))
         (str (proof-shell-strip-eager-annotations start end))
         (strnotrailingspace
          (coq-remove-starting-blanks (proof-strip-whitespace-at-end str))))
    (pg-response-display-with-face strnotrailingspace))) ; face


;; slight modification of proof-script-generic-parse-cmdend (one of the
;; candidate for proof-script-parse-function), to allow "{" and "}" to be
;; command terminator when the command is empty. TO PLUG: swith the comment
;; below and rename coq-script-parse-function2 into coq-script-parse-function
(defun coq-script-parse-function ()
  "For `proof-script-parse-function' if `proof-script-command-end-regexp' set."
  (coq-script-parse-cmdend-forward))

;;;;;;;;;;;;;;;;;;;;;;;;;; End of "{" and "} experiments ;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;; Freeze buffers ;;;;;;;;;;;;
;; For storing output of respnse and goals buffers into a permanent buffer.

(defun coq-clone-buffer-response-mode (s &optional erase)
  (let ((already-existing (get-buffer s))
        (nb (get-buffer-create s)))
    (save-window-excursion
      (switch-to-buffer nb)
      (unless already-existing
        (coq-response-mode)
        (read-only-mode 0))
      (goto-char (point-min))
      (insert "\n************************************\n")
      (goto-char (point-min)))
    (if erase (copy-to-buffer nb (point-min) (point-max))
      (append-to-buffer nb (point-min) (point-max)))
    ;; (set-window-point window pos)
    nb))

;; copy the content of proof-response-buffer into the "response-freeze" buffer,
;; resetting its content if ERASE non nil.
(defun proof-store-buffer-win (buffer &optional erase)
  (proof-with-current-buffer-if-exists buffer
    (let ((newbuffer nil))
      (set-buffer buffer)
      (setq newbuffer (coq-clone-buffer-response-mode "*response-freeze*" erase))
      (let ((win (display-buffer-other-frame newbuffer))
            (win-point-min (save-window-excursion
                             (switch-to-buffer-other-frame newbuffer)
                             (point-min))))
      (set-window-point win win-point-min)))))

(defun proof-store-response-win (&optional erase)
  (interactive "P")
  (proof-store-buffer-win proof-response-buffer erase))

(defun proof-store-goals-win (&optional erase)
  (interactive "P")
  (proof-store-buffer-win proof-goals-buffer erase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
  "Extract info from the coq prompt S.  See `coq-last-prompt-info-safe'."
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
  "Return a list with all informations from the last prompt.
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
  "Return the 'goalcmd flag of the SPAN."
  (span-property span 'goalcmd))

(defsubst coq-set-span-goalcmd (span val)
  "Set the 'goalcmd flag of the SPAN to VAL."
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
If locked span already has a state number, then do nothing. Also updates
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
  "Backtrack to SPAN.  Using the \"BackTo n\" coq command."
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
  "Instanciation for `pg-topterm-goalhyplit-fn', see there."
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
  "Instanciation for `proof-state-preserving-p', see there."
  ;; (or
   (proof-string-match coq-non-retractable-instruct-regexp cmd))
  ;; (and
  ;;  (not (proof-string-match coq-retractable-instruct-regexp cmd))
  ;;  (or
  ;;   (message "Unknown command, hopes this won't desynchronize ProofGeneral")
  ;;   t))))

(defun coq-cmd-prevents-proof-omission (cmd)
  "Instanciation for `proof-script-cmd-prevents-proof-omission'.
This predicate decides whether a command inside a proof might
have effects outside the proof, which would prohibit omitting the
proof, see `proof-script-omit-proofs'.

Commands starting lower case are deemed as tactics that have
proof local effect only and so are bullets and braces. Everything
else is checked against the STATECH field in the coq syntax data
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

(defconst notation-print-kinds-table
  '(("Print Scope(s)" 0) ("Print Visibility" 1))
  "Enumerates the different kinds of notation information one can ask to coq.")

(defun coq-PrintScope ()
  "Show information on notations.  Coq specific."
  (interactive)
  (let*
      ((mods
        (completing-read "Infos on notation (TAB to see list): "
                         notation-print-kinds-table))
       (s (read-string  "Name (empty for all): "))
       ;; (all (string-equal s ""))
       )
    (cond
     ((and (string-equal mods "Print Scope(s)") (string-equal s ""))
      (proof-shell-invisible-command (format "Print Scopes.")))
     (t
      (proof-shell-invisible-command (format "%s %s ." mods s))))))

(defun coq-remove-trailing-dot (s)
  "Return the string S without its trailing \".\" if any.
Return nil if S is nil."
  (if (and s (string-match "\\.\\'" s))
      (substring s 0 (- (length s) 1))
    s))

(defun coq-remove-heading-quote (s)
  "Return the string S without its heading \"\'\" if any.
Return nil if S is nil."
  (if (and s (string-match "\\`'" s))
      (substring s 1 (length s))
    s))

(defun coq-clean-id-at-point (s)
  (coq-remove-heading-quote (coq-remove-trailing-dot s)))

(defun coq-is-symbol-or-punct (c)
  "Return non nil if character C is a punctuation or a symbol constituent.
If C is nil, return nil."
  (when c
    (or (equal (char-syntax c) ?\.) (equal (char-syntax c) ?\_))))

(defun coq-grab-punctuation-left (pos)
  "Return a string made of punctuations chars found immediately before position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-before currpos))
      (setq res (concat (char-to-string (char-before currpos)) res))
      (setq currpos (- currpos 1)))
    res))


(defun coq-grab-punctuation-right (pos)
  "Return a string made of punctuations chars found immediately after position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-after currpos))
      (setq res (concat res (char-to-string (char-after currpos))))
      (setq currpos (+ currpos 1)))
    res))

(defun coq-notation-at-position (pos)
  "Return the notation at current point.
Support dot.notation.of.modules."
  (coq-with-altered-syntax-table
   (when (or (coq-grab-punctuation-left pos) (coq-grab-punctuation-right pos))
     (concat (coq-grab-punctuation-left pos)
             (coq-grab-punctuation-right pos)))))

(defun coq-string-starts-with-symbol (s)
  (eq 0 (string-match "\\s_" s)))

;; remove trailing dot if any.
(defun coq-id-at-point ()
  "Return the identifier at current point.
Support dot.notation.of.modules."
  (coq-with-altered-syntax-table
   (let* ((symb (cond
                 ((fboundp 'symbol-near-point) (symbol-near-point))
                 ((fboundp 'symbol-at-point) (symbol-at-point))))
          (symbclean (when symb (coq-clean-id-at-point (symbol-name symb)))))
     (when (and symb (not (zerop (length symbclean))))
       symbclean))))


(defun coq-id-or-notation-at-point ()
  (or (coq-id-at-point)
      (let ((notation (coq-notation-at-position (point))))
        (if notation (concat "\"" notation "\"") ""))))

(defcustom coq-remap-mouse-1 nil
  "Whether Coq mode should remap mouse button 1 to Coq queries.

This overrides the default global binding of (control mouse-1) and
\(shift mouse-1) (buffers and faces menus).  Hence it is nil by
default."
  :type 'boolean
  :group 'coq)


;; On a mouse event, try to query the id at point clicked.
(defun coq-id-under-mouse-query (event)
  "Query the prover about the identifier or notation near mouse click EVENT.
This is mapped to control/shift mouse-1, unless coq-remap-mouse-1
is nil (t by default)."
  (interactive "e")
  (save-selected-window
    (save-excursion
      (mouse-set-point event)
      (let* ((id (coq-id-at-point))
             (notat (coq-notation-at-position (point)))
             (modifs (event-modifiers event))
             (shft (member 'shift modifs))
             (ctrl (member 'control modifs))
             (cmd (when (or id notat)
                    (if (and ctrl shft) (if id "Check" "Locate")
                      (if shft (if id "About" "Locate")
                        (if ctrl (if id "Print" "Locate")))))))
        (proof-shell-invisible-command
         (format (concat  cmd " %s .")
                 ;; Notation need to be surrounded by ""
                 (if id id (concat "\"" notat "\""))))))))

(defun coq-guess-or-ask-for-string (s &optional dontguess)
  "Asks for a coq identifier with message S.
If DONTGUESS is non nil, propose a default value as follows:

If region is active, propose its containt as default value.

Otherwise propose identifier at point if any."
  (let* ((guess
          (cond
           (dontguess nil)
           ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           (t (coq-id-or-notation-at-point)))))
    (read-string
     (if guess (concat s " (default " guess "): ") (concat s ": "))
     nil 'proof-minibuffer-history guess)))


(defun coq-ask-do (ask do &optional dontguess postformatcmd wait)
  "Ask for an ident and print the corresponding term."
  (let* ((cmd) (postform (if (eq postformatcmd nil) 'identity postformatcmd)))
    (proof-shell-ready-prover)
    (setq cmd (coq-guess-or-ask-for-string ask dontguess))
    (proof-shell-invisible-command
     (format (concat do " %s .") (funcall postform cmd))
     wait)))


(defun coq-flag-is-on-p (testcmd)
  (proof-shell-ready-prover)
  (proof-shell-invisible-command (format testcmd) 'wait)
  (let ((resp proof-shell-last-response-output))
    (string-match "is on\\>" resp)))

(defun coq-command-with-set-unset (setcmd cmd unsetcmd &optional postformatcmd testcmd)
  "Play commands SETCMD then CMD and then silently UNSETCMD.
The last UNSETCMD is performed with tag 'empty-action-list so that it
does not trigger ‘proof-shell-empty-action’ (which does \"Show\" at
the time of writing this documentation)."
  (let* ((postform (if (eq postformatcmd nil) 'identity postformatcmd))
         (flag-is-on (and testcmd (coq-flag-is-on-p testcmd))))
    ;; We put 'empty-action-list tags on all three commands since we don't want
    ;; to trigger "Show" or anything that we usually insert after a group of
    ;; commands.
    (unless flag-is-on (proof-shell-invisible-command
                        (format " %s ." (funcall postform setcmd))
                        nil nil 'no-response-display 'empty-action-list))
    (proof-shell-invisible-command
     (format " %s ." (funcall postform cmd)) 'wait nil 'empty-action-list)
    (unless flag-is-on (proof-shell-invisible-command
                        (format " %s ." (funcall postform unsetcmd))
                        'waitforit  nil 'no-response-display 'empty-action-list))))

(defun coq-ask-do-set-unset (ask do setcmd unsetcmd
                                 &optional dontguess postformatcmd _tescmd)
  "Ask for an ident id and execute command DO in SETCMD mode.
More precisely it executes SETCMD, then DO id and finally silently
UNSETCMD.  See `coq-command-with-set-unset'."
  (let* ((cmd)
         ;; (postform (if (eq postformatcmd nil) 'identity postformatcmd tescmd))
         )
    (proof-shell-ready-prover)
    (setq cmd (coq-guess-or-ask-for-string ask dontguess))
    (coq-command-with-set-unset setcmd (concat do " " cmd) unsetcmd postformatcmd)))


(defun coq-ask-do-show-implicits (ask do &optional dontguess postformatcmd)
  "Ask for an ident and print the corresponding term."
  (coq-ask-do-set-unset ask do
                        "Set Printing Implicit"
                        "Unset Printing Implicit"
                        dontguess postformatcmd
                        "Test Printing Implicit"))

(defun coq-ask-do-show-all (ask do &optional dontguess postformatcmd)
  "Ask for an ident and print the corresponding term."
  (coq-ask-do-set-unset ask do
                        "Set Printing All"
                        "Unset Printing All"
                        dontguess postformatcmd
                        "Test Printing All"))


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

(defun coq-SearchIsos ()
  "Search a term whose type is isomorphic to given type.
This is specific to `coq-mode'."
  (interactive)
  (coq-ask-do
   "SearchPattern (parenthesis mandatory), ex: (?X1 + _ = _ + ?X1)"
   "SearchPattern" nil))

(defun coq-SearchRewrite ()
  (interactive)
  (coq-ask-do "SearchRewrite" "SearchRewrite" nil))

(defun coq-Print (withprintingall)
  "Ask for an ident and print the corresponding term.
With flag Printing All if some prefix arg is given (C-u)."
  (interactive "P")
  (if withprintingall
      (coq-ask-do-show-all "Print" "Print")
    (coq-ask-do "Print" "Print")))

(defun coq-Print-Ltac ()
  "Ask for an ident and print the corresponding Ltac term."
  (interactive)
  (coq-ask-do "Print Ltac" "Print Ltac"))

(defun coq-Print-with-implicits ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-ask-do-show-implicits "Print" "Print"))

(defun coq-Print-with-all ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-ask-do-show-all "Print" "Print"))

(defun coq-About (withprintingall)
  "Ask for an ident and print information on it."
  (interactive "P")
  (if withprintingall
      (coq-ask-do-show-all "About" "About")
    (coq-ask-do "About" "About")))

(defun coq-About-with-implicits ()
  "Ask for an ident and print information on it."
  (interactive)
  (coq-ask-do-show-implicits "About" "About"))

(defun coq-About-with-all ()
  "Ask for an ident and print information on it."
  (interactive)
  (coq-ask-do-show-all "About" "About"))


(defun coq-LocateConstant ()
  "Locate a constant."
  (interactive)
  (coq-ask-do "Locate" "Locate"))

(defun coq-LocateLibrary ()
  "Locate a library."
  (interactive)
  (coq-ask-do "Locate Library" "Locate Library"))

(defun coq-LocateNotation ()
  "Locate a notation.  Put it automatically into quotes.
This is specific to `coq-mode'."
  (interactive)
  (coq-ask-do
   "Locate notation (ex: \'exists\' _ , _)" "Locate"
   ))

(defun coq-set-undo-limit (undos)
  (proof-shell-invisible-command (format "Set Undo %s ." undos)))

(defun coq-Pwd ()
  "Display the current Coq working directory."
  (interactive)
  (proof-shell-invisible-command "Pwd."))

(defun coq-Inspect ()
  (interactive)
  (coq-ask-do "Inspect how many objects back?" "Inspect" t))

(defun coq-PrintSection()
  (interactive)
  (coq-ask-do "Print Section" "Print Section" t))

(defun coq-Print-implicit ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-ask-do "Print Implicit" "Print Implicit"))

(defun coq-Check (withprintingall)
  "Ask for a term and print its type.
With flag Printing All if some prefix arg is given (C-u)."
  (interactive "P")
  (if withprintingall
      (coq-ask-do-show-all "Check" "Check")
    (coq-ask-do "Check" "Check")))

(defun coq-Check-show-implicits ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-implicits "Check" "Check"))

(defun coq-Check-show-all ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-all "Check" "Check"))

(defun coq-get-response-string-at (&optional pt)
  "Go forward from PT until reaching a 'response property, and return it.
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

(defun coq-Show (withprintingall)
  "Ask for a number I and show the Ith goal.
Ask for a number I and show the ith current goal. With non-nil prefix
argument and not on the locked span, show the goal with flag
Printing All set."
; Disabled:
;  "Ask for a number i and show the ith goal, or show ancient goal.
;If point is on a locked span, show the corresponding coq
;output (i.e. for tactics: the goal after the tactic). Otherwise
;ask for a number i and show the ith current goal. With non-nil
;prefix argument and not on the locked span, show the goal with
;flag Printing All set."
;
  (interactive "P")
  ;; Disabling this as this relies on 'response attribute that is empty when
  ;; the command was processed silently. We should first have a coq command
  ;; asking to print the goal at a given state.
  (if (proof-in-locked-region-p)
      (let ((s (coq-get-response-string-at)))
        (if (zerop (length (coq-get-response-string-at)))
            (message "Cannot show the state at this point: Coq was silent during this command.")
          (set-buffer proof-response-buffer)
          (let ((inhibit-read-only 'titi))
            (pg-response-display s)
            (proof-display-and-keep-buffer proof-response-buffer)
            (coq-optimise-resp-windows))))
    (if withprintingall
        (coq-ask-do-show-all "Show goal number" "Show" t)
      (coq-ask-do "Show goal number" "Show" t))))

(defun coq-Show-with-implicits ()
  "Ask for a number i and show the ith goal."
  (interactive)
  (coq-ask-do-show-implicits "Show goal number" "Show" t))

(defun coq-Show-with-all ()
  "Ask for a number i and show the ith goal."
  (interactive)
  (coq-ask-do-show-all "Show goal number" "Show" t))

;; Since Printing Width is a synchronized option in coq (?) it is retored
;; silently to a previous value when retracting. So we reset the stored width
;; when retracting, so that it will be auto-adapted at the next command. Not
;; perfect: we have to forward one step to see the effect.

;; FIXME: hopefully this will eventually become a non synchronized option and
;; we can remove this.
;; This obeyx coq-auto-adapt-printing-width
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
  "Return the list of commands to send to Coq after CMD
if it is the last command of the action list.
If CMD is tagged with 'empty-action-list then this function won't
be called and no command will be sent to Coq.
Note: the last command added if `coq-show-proof-stepwise' is set
should match the `coq-show-proof-diffs-regexp'."
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
     ;; If we go back in the buffer and the number of abort is less than
     ;; the number of nested goals, then Unset Silent and Show the goal
     (and (string-match-p "BackTo\\s-" cmd)
          (> (length coq-last-but-one-proofstack) coq--retract-naborts)))
    `("Unset Silent."
      ,(if (coq--post-v810) (coq-diffs) "Show.")
      . ,(coq--show-proof-stepwise-cmds)))

   ((or
     ;; If we go back in the buffer and not in the above case, then only Unset
     ;; silent (there is no goal to show). Still, we need to "Set Diffs" again
     (string-match-p "BackTo\\s-" cmd))
    (if (coq--post-v810)
        (list "Unset Silent." (coq-diffs) )
      (list "Unset Silent.")))
   ((or
     ;; If starting a proof, Show Proof if need be
     (coq-goal-command-str-p cmd)
     ;; If doing (not closing) a proof, Show Proof if need be
     (and (not (string-match-p coq-save-command-regexp-strict cmd))
          (> (length coq-last-but-one-proofstack) 0)))
    (coq--show-proof-stepwise-cmds))))

;; This does not Set Printing Width, it rather tells pg to do that before each
;; command (if necessary)
(defpacustom auto-adapt-printing-width t
  "If non-nil, adapt automatically printing width of goals window.
Each time the user sends a bunch of commands to Coq, check if the
width of the goals window changed, and adapt coq printing width.
WARNING: If several windows are displaying the goals buffer, one
is chosen arbitrarily.  WARNING 2: when backtracking the printing
width is synchronized by coq (?!)."
  :type 'boolean
  :safe 'booleanp
  :group 'coq)

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
        (proof-shell-invisible-command (format "Set Printing Width %S." (- wdth 1)) t)
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

;;;;;;;;;; Hypothesis tracking ;;;;;;;;;;
;;; Facilities to build overlays for hyp names and hyp+type + hypcross
;;; ((un)folding buttons).
(defvar coq-hypname-map (make-sparse-keymap) "Keymap for visible hypothesis.")
(defvar coq-hypcross-map (make-sparse-keymap) "Keymap for visible hypothesis.")

(defvar coq-hypcross-hovering-help t
  "Whether hyps fold cross pops up a help when hovered.")

(defvar coq-hyps-positions nil
  "The list of positions of hypothesis in the goals buffer.
Maintained by a hook in `proof-shell-handle-delayed-output-hook'.")

;; Hyps name are clickable and have a special keymap.
(defun coq-make-hypname-overlay (beg end h buf)
  (let ((ov (make-overlay beg end buf)))
    (when ov
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'is-hypname t)
      (overlay-put ov 'hyp-name h)
      (overlay-put ov 'keymap coq-hypname-map))
    ov))

;; Hyps types are clickable only when folded
(defun coq-make-hyp-overlay (beg end h buf)
  (let ((ov (make-overlay beg end buf)))
    (when ov
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'is-hyp t)
      (overlay-put ov 'hyp-name h))
    ov))

(defun coq-hypcross-unfolded-string()
  (propertize
   "-"
   'face 'coq-button-face
   'mouse-face 'coq-button-face-active))

(defun coq-hypcross-folded-string()
  (propertize
   "+"
   'face 'coq-button-face-pressed
   'mouse-face 'coq-button-face-active))

;; hypcross is displayerd with a "-" when unfolded and a "+" when unfolded.
;; It is highlighted when hovered, is clickable and have a special
;; keymap (f toggles folding, h toggles highlighting...).
;; PC: using button faces of make-button makes the button larger than a
;; characters, which messes columns. So we use simple background colors in
;; coq-hyp...faces
(defun coq-make-hypcross-overlay (beg end h buf)
  (let ((ov (make-overlay beg end buf)))
    (when ov
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'is-hypcross t)
      (overlay-put ov 'hyp-name h)
      (overlay-put ov 'keymap coq-hypcross-map)
      (overlay-put ov 'display (coq-hypcross-unfolded-string))
      ;(overlay-put ov 'face 'coq-button-face)
      ;(overlay-put ov 'mouse-face 'coq-button-face-pressed)
      ;(overlay-put ov 'width .6)
      ;(overlay-put ov 'height -1)
      (when (eq coq-hypcross-hovering-help t)
        (overlay-put ov 'help-echo "MOUSE-1: (un)fold ; MOUSE-2 copy hyp name at point")))
    ov))

;; Once we have created the 3 overlays, each recieves a reference to the 2
;; others.
(defun coq-hyp-overlay-build-cross-refs (hypnameov hypov crosshypov)
  (overlay-put hypnameov 'hypcross-ov crosshypov)
  (overlay-put hypov 'hypcross-ov crosshypov)
  (overlay-put hypnameov 'hyp-ov hypov)
  (overlay-put crosshypov 'hyp-ov hypov)
  (overlay-put hypov 'hypname-ov hypnameov)
  (overlay-put crosshypov 'hypname-ov hypnameov))

(defun coq-build-hyp-overlay (hyp-positions buf)
  (let* ((names (car hyp-positions))
         (fstname (car names))
         (positions (cdr hyp-positions))
         (begcross (car positions))
         (beghypname (cadr positions))
         (endhypname (cl-caddr positions))
         (beg (cl-cadddr positions))
         (end (cl-cadddr (cdr positions))))
    (let ((hypnameov (coq-make-hypname-overlay beghypname endhypname fstname buf))
          (hypov (coq-make-hyp-overlay beg end fstname buf))
          (crosshypov (coq-make-hypcross-overlay begcross (+ 1 begcross) fstname buf))
          res)
      (coq-hyp-overlay-build-cross-refs hypnameov hypov crosshypov)
      (mapc
       (lambda (s)
         (setq res
               (cons
                (cons (substring-no-properties s)
                      (cons hypov (cons hypnameov (cons crosshypov nil)) ))
                res)))
       names)
      res)))

(defun coq-build-hyps-overlays (hyp-positions buf)
  (let ((res))
    (mapc (lambda (x) (let ((lassoc (coq-build-hyp-overlay x buf)))
                        (setq res (append lassoc res))))
          hyp-positions)
    res))

; builds the list of names from one hyp position (which may contained several
; hyp names)
(defun coq-build-hyp-name (hyp-positions)
  (let* ((names (car hyp-positions))
         res)
   (mapc (lambda (s) (setq res (cons (substring-no-properties s) res))) names)
    res))

;; Build the list of hyps names
(defun coq-build-hyps-names (hyp-positions)
  (let ((res))
    (mapc (lambda (x) (let ((lassoc (coq-build-hyp-name x)))
                        (setq res (append lassoc res))))
          hyp-positions)
    res))


(defun coq-detect-hyps-in-goals ()
  "Detect all hypotheses displayed in goals buffer and create overlays.
Three overlays are created for each hyp: (hypov hypnameov hypcrossov),
respectively for the whole hyp, only its name and the overlay for
fold/unfold cross.  Return the list of mappings hypname -> overlays."
  ;; FIXME: Consolidate the `with-current-buffer proof-goals-buffer's.
  (let*
      ((fsthyp-pos (with-current-buffer proof-goals-buffer
                     (coq-find-first-hyp)))
       (fsthyp-ov (when fsthyp-pos (with-current-buffer proof-goals-buffer
                                     (overlays-at fsthyp-pos))))
       ; is there at least one hyp overlay there?
       (fsthyp-hypov (when fsthyp-ov
                       (cl-some (lambda(x) (overlay-get x 'hyp-name))
                                fsthyp-ov))))
    (if fsthyp-hypov coq-hyps-positions ;overlays are already there
      (with-current-buffer proof-goals-buffer
        (coq-build-hyps-overlays (coq-detect-hyps-positions-in-goals)
                                 proof-goals-buffer)))))

(defun coq-find-hyp-overlay (h)
  (cadr (assoc h coq-hyps-positions)))

;;;;;;;;;;;;;; Highlighting hypothesis ;;;;;;;;
;; Feature: highlighting of hypothesis that remains when the script is played
;; (and goals buffer is updated).

;; On by default. This only works with the Search function for now.
(defvar coq-highlight-hyps-cited-in-response t
  "If non-nil, try to highlight in goals buffers hyps cited in response.")

(defvar coq-highlight-hyps-sticky nil
  "If non-nil, try to make hypothesis highlighting sticky.
The is try to re-highlight the hypothesis with same names
after a refreshing of the response buffer.")

;; We maintain a list of hypothesis names that must be highlighted at each
;; regeneration of goals buffer.
(defvar coq-highlighted-hyps nil
  "List of hypothesis names that must be highlighted.
These are rehighlighted at each regeneration of goals buffer
using a hook in `proof-shell-handle-delayed-output-hook'.")

(defvar coq-highlighted-hyps-bg "gray"
  "background color for highlighted hyps.")


(defun coq-highlight-hyp-aux (h)
  "Auxiliary function, use `coq-highlight-hyp' for sticky highlighting.
Unless you want the highlighting to disappear after the goals
buffer is updated."
  (let ((hyp-overlay (coq-find-hyp-overlay h)))
    (when hyp-overlay
      (overlay-put hyp-overlay 'face `(:background ,coq-highlighted-hyps-bg)))))

(defun coq-unhighlight-hyp-aux (h)
  "Auxiliary function, use `coq-unhighlight-hyp' for sticky highlighting.
Unless you want the highlighting to disappear after the goals
buffer is updated."
  (let* ((hyp-overlay (coq-find-hyp-overlay h)))
    (when hyp-overlay
      (overlay-put hyp-overlay 'face nil))))

(defun coq-highlight-hyp (h)
  "Highlight hypothesis named H (sticky).
use `coq-unhighlight-hyp' to unhighlight."
  (unless (member h coq-highlighted-hyps)
    (setq coq-highlighted-hyps (cons h coq-highlighted-hyps)))
  (coq-highlight-hyp-aux h))

(defun coq-unhighlight-hyp (h)
  "Unhighlight hypothesis named H."
  (when (member h coq-highlighted-hyps)
    (setq coq-highlighted-hyps (delete h coq-highlighted-hyps))
    (coq-unhighlight-hyp-aux h)))

(defun coq-highlight-hyps (lh)
  (mapc #'coq-highlight-hyp lh))

(defun coq-unhighlight-hyps (lh)
  (mapc #'coq-unhighlight-hyp-aux lh))

(defun coq-highlight-selected-hyps ()
  "Highlight all hyps stored in `coq-highlighted-hyps'.
This used in hook to rehilight hypothesis after goals buffer is
updated."
  (interactive)
  (coq-highlight-hyps coq-highlighted-hyps))

(defun coq-unhighlight-selected-hyps ()
  "Unhighlight all hyps stored in `coq-highlighted-hyps'.
This used before updating `coq-highlighted-hyps' with a new set
of hypothesis to highlight."
  (interactive)
  (coq-unhighlight-hyps coq-highlighted-hyps)
  (setq coq-highlighted-hyps nil))

(defun coq-get-hyps-cited-in-response ()
  "Returns locations of hyps in goals buffer that are cited in response buffer.
See `coq-highlight-hyps-cited-in-response' and `Search'."
  (let* ((hyps-cited-pos (coq-detect-hyps-positions proof-response-buffer))
         (hyps-cited (coq-build-hyps-names hyps-cited-pos)))
    (cl-remove-if-not
     (lambda (e)
       (cl-some;seq-find
        (lambda (f)
          (string-equal (car e) f))
        hyps-cited))
     coq-hyps-positions)))

(defun coq-highlight-hyps-cited-in-response ()
  "Highlight hypothesis cited in response buffer.
Highlight always takes place in goals buffer."
  (let  ((inters (coq-get-hyps-cited-in-response)))
    (coq-unhighlight-selected-hyps)
    (setq coq-highlighted-hyps (mapcar #'car inters))
    (coq-highlight-selected-hyps)))

(defun coq-toggle-highlight-hyp (h)
  "Toggle the highlighting status of hypothesis H.
See  `coq-highlight-hyp'."
  (interactive "sHypothesis name to highlight:")
  (if (member h coq-highlighted-hyps)
      (coq-unhighlight-hyp h)
    (coq-highlight-hyp h)))

(defun coq-toggle-highlight-hyp-at (pt)
  "Toggle hiding the hypothesis at point."
  (interactive)
  (let* ((ov
          (or (car (coq-overlays-at pt 'hyp-name))
              ;; we may be between hypname and hyp, so skip backward to
              ;; something meaningful
              (save-excursion
                (goto-char pt)
                (search-backward-regexp "[^ :=]\\|\n")
                (car (coq-overlays-at (point) 'hyp-name)))))
         (hypname (and ov
                       (overlay-get ov 'hyp-name))))
    (when hypname
      (if (member hypname coq-highlighted-hyps)
          (coq-unhighlight-hyp hypname)
        (coq-highlight-hyp hypname)))))

(defun coq-toggle-highlight-hyp-at-point ()
  (interactive)
  (coq-toggle-highlight-hyp-at (point)))


(defun coq-insert-at-point-hyp-at-mouse (event)
  (interactive "e")
  (let ((hyp-name (save-excursion
                    (with-current-buffer proof-goals-buffer
                      (save-excursion
                        (overlay-get (car (overlays-at (posn-point (event-start event))))
                                     'hyp-name))))))
    (insert hyp-name)))

;;;;;;;;;;; Hiding Hypothesis  ;;;;;;;;;;;
(defvar coq-hidden-hyp-map (make-sparse-keymap)
  "Keymap for hidden hypothesis.")

(defvar coq-hidden-hyps nil
  "List of hypothesis that should be hidden in goals buffer.
These are re-hidden at each regeneration of goals buffer
using a hook in `proof-shell-handle-delayed-output-hook'.")


(defun coq-toggle-fold-hyp-at (pt)
  "Toggle hiding the hypothesis at point."
  (interactive)
  (let* ((ov (car (coq-overlays-at pt 'hyp-name)))
         (hypname (and ov (overlay-get ov 'hyp-name))))
    (when hypname
      (if (member hypname coq-hidden-hyps)
          (coq-unfold-hyp hypname)
        (coq-fold-hyp hypname)))))

(defun coq-toggle-fold-hyp-at-point ()
  (interactive)
  (coq-toggle-fold-hyp-at (point)))

(defun coq-toggle-fold-hyp-at-mouse (event)
  "Toggle hiding the hypothesis at mouse click.
Used on hyptohesis overlays in goals buffer mainly."
  (interactive "e")
  (save-excursion
    (with-current-buffer proof-goals-buffer
      (save-excursion (coq-toggle-fold-hyp-at (posn-point (event-start event)))))))

(defun coq-configure-hyp-overlay-hidden (hyp-overlay h)
  (when hyp-overlay
    (let* ((lgthhyp (- (overlay-end hyp-overlay) (overlay-start hyp-overlay)))
           (prefx (make-string (min 8 lgthhyp) ?.))
           (hypcross-ov (overlay-get hyp-overlay 'hypcross-ov)))
      (overlay-put
       hyp-overlay 'display
       prefx ;(propertize prefx 'keymap coq-hidden-hyp-map)
       )
      (overlay-put hyp-overlay 'evaporate t)
      (overlay-put hyp-overlay 'mouse-face 'proof-command-mouse-highlight-face)
      (overlay-put hyp-overlay 'help-echo "MOUSE-1: unfold; MOUSE-2 copy name at point")
      (overlay-put hyp-overlay 'hyp-name h)
      (overlay-put hyp-overlay 'keymap coq-hidden-hyp-map)
      (overlay-put hypcross-ov 'display (coq-hypcross-folded-string)))))

(defun coq-configure-hyp-overlay-visible (hyp-overlay _h)
  (when hyp-overlay
    (overlay-put hyp-overlay 'display nil)
    (overlay-put hyp-overlay 'evaporate t)
    (overlay-put hyp-overlay 'mouse-face nil)
    (overlay-put hyp-overlay 'help-echo nil)
    (overlay-put hyp-overlay 'keymap nil)
    (overlay-put (overlay-get hyp-overlay 'hypcross-ov)
                 'display (coq-hypcross-unfolded-string))))

(defun coq-fold-hyp-aux (h)
  "Fold hypothesis H's type from the context temporarily.
Displays \".......\" instead. This function relies on variable
coq-hyps-positions. The hiding is cancelled as soon as the goals
buffer is changed, consider using `coq-fold-hyp' if you want the
hiding to be maintain when scripting/undoing."
  (let ((hyp-overlay (coq-find-hyp-overlay h)))
    (when hyp-overlay (coq-configure-hyp-overlay-hidden hyp-overlay h))))

(defun coq-fold-hyp (h)
  "Fold hypothesis H's type from the context durably.
\(displays \".......\" instead).  This function relies on variable
‘coq-hyps-positions’.  The hiding maintained as the goals buffer is
changed, thanks to a hook on `proof-shell-handle-delayed-output-hook',
consider using `coq-fold-hyp' if you want the hiding to be maintain
when scripting/undoing."
  (interactive)
  (unless (member h coq-hidden-hyps)
    (setq coq-hidden-hyps (cons h coq-hidden-hyps))
    (coq-fold-hyp-aux h)))

(defun coq-unfold-hyp-aux (h)
"Unfold hypothesis H temporarily.
See `coq-fold-hyp-aux'."
    (let ((hyp-overlay (coq-find-hyp-overlay h)))
      (coq-configure-hyp-overlay-visible hyp-overlay h)))

(defun coq-unfold-hyp (h)
"Unfold hypothesis H durably.
See `coq-fold-hyp'."
  (interactive)
  (when (member h coq-hidden-hyps)
    (setq coq-hidden-hyps (delete h coq-hidden-hyps))
    (coq-unfold-hyp-aux h)))

(defun coq-unfold-hyp-list (lh)
    "Fold the type of hypothesis in LH temporarily.
See `coq-unfold-hyp-aux'."
  (mapc #'coq-unfold-hyp-aux lh))

(defun coq-fold-hyp-list (lh)
    "Fold the type of hypothesis in LH temporarily.
See `coq-fold-hyp-aux'."
  (mapc #'coq-fold-hyp-aux lh))

(defun coq-fold-hyps ()
    "Fold the type of hypothesis in LH durably.
See `coq-fold-hyp'."
  (interactive)
  (coq-fold-hyp-list coq-hidden-hyps))


(defun coq-unfold-hyps ()
    "Unfold the type of hypothesis in LH durably.
See `coq-unfold-hyp'."
  (interactive)
  (coq-unfold-hyp-list coq-hidden-hyps)
  (setq coq-hidden-hyps nil))

(defun coq-toggle-fold-hyp (h)
  "Toggle the hiding status of hypothesis H (asked interactively).
See  `coq-fold-hyp'."
  (interactive "sHypothesis to fold: ")
  (if (member h coq-hidden-hyps)
      (coq-unfold-hyp h)
    (coq-fold-hyp h)))
;;;;;;;

(proof-definvisible coq-PrintHint "Print Hint.")

;; Items on show menu
(proof-definvisible coq-show-tree "Show Tree.")
(proof-definvisible coq-show-proof "Show Proof.")
(proof-definvisible coq-show-conjectures "Show Conjectures.")
(proof-definvisible coq-show-intros "Show Intros.") ; see coq-insert-intros below
(proof-definvisible coq-set-printing-implicit "Set Printing Implicit.")
(proof-definvisible coq-unset-printing-implicit "Unset Printing Implicit.")
(proof-definvisible coq-set-printing-all "Set Printing All.")
(proof-definvisible coq-unset-printing-all "Unset Printing All.")
(proof-definvisible coq-set-printing-parentheses "Set Printing Parentheses.")
(proof-definvisible coq-unset-printing-parentheses "Unset Printing Parentheses.")
(proof-definvisible coq-set-printing-notations "Set Printing Notations.")
(proof-definvisible coq-unset-printing-notations "Unset Printing Notations.")
(proof-definvisible coq-set-printing-synth "Set Printing Synth.")
(proof-definvisible coq-unset-printing-synth "Unset Printing Synth.")
(proof-definvisible coq-set-printing-coercions "Set Printing Coercions.")
(proof-definvisible coq-unset-printing-coercions "Unset Printing Coercions.")
(proof-definvisible coq-set-printing-compact-contexts "Set Printing Compact Contexts.")
(proof-definvisible coq-unset-printing-compact-contexts "Unset Printing Compact Contexts.")
(proof-definvisible coq-set-printing-unfocused "Set Printing Unfocused.")
(proof-definvisible coq-unset-printing-unfocused "Unset Printing Unfocused.")
(proof-definvisible coq-set-printing-universes "Set Printing Universes.")
(proof-definvisible coq-unset-printing-universes "Unset Printing Universes.")
(proof-definvisible coq-set-printing-wildcards "Set Printing Wildcard.")
(proof-definvisible coq-unset-printing-wildcards "Unset Printing Wildcard.")
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Holes mode switch
;; TODO: have this plugged agian when we have abbreviation without holes
;; For now holes are always enabled.
;(defpacustom use-editing-holes t
;  "Enable holes for editing."
;  :type 'boolean)


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
    (let ((pt (progn (save-excursion (forward-line -1) (point)))))
      (save-excursion
        (re-search-backward "^TcDebug" pt t)
        (re-search-backward "<infomsg>\\|^TcDebug\\|^</prompt>" nil t)
        (when (looking-at "<infomsg>")
          (let ((pt2 (point)))
            (re-search-backward "Goal:\\|^TcDebug\\|^</prompt>" nil t)
            (when (looking-at "Goal")
              (pg-goals-display (buffer-substring (point) pt2) nil))))))))

;; overwrite the generic one, interactive prompt is Debug mode;; try to display
;; the debug goal in the goals buffer.
(defun proof-shell-process-interactive-prompt-regexp ()
  "Action taken when `proof-shell-interactive-prompt-regexp' is observed."
  (when (and (proof-shell-live-buffer)
	     ; not already visible
	     t)
    (switch-to-buffer proof-shell-buffer)
    (coq-display-debug-goal)
    (message "Prover expects input in %s buffer (if debug mode: h<return> for help))" proof-shell-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar coq-pg-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map proof-mode-map)

    ;;(define-key coq-mode-map coq-double-hit-hot-key 'coq-terminator-insert)

    ;; Setting the new mapping for terminator, overrides the following in
    ;; proof-script:
    ;; (define-key proof-mode-map (vector (aref proof-terminal-string 0))
    ;;             #'proof-electric-terminator)
    ;;(define-key proof-mode-map (kbd coq-double-hit-hot-key)
    ;;            #'coq-terminator-insert)
    (define-key map (kbd ".") #'coq-terminator-insert)
    ;;(define-key map (kbd ";") #'coq-terminator-insert) ; for french keyboards
    map))

;;;###autoload
(defun coq-pg-setup ()
  (set-keymap-parent (current-local-map) coq-pg-mode-map)
  
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
  (setq proof-guess-command-line #'coq-guess-command-line)
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

  (setq proof-shell-start-silent-cmd "Set Silent."
        proof-shell-stop-silent-cmd "Unset Silent.")

  ;; prooftree config
  (setq
   proof-tree-configured (coq--post-v810)
   proof-tree-get-proof-info 'coq-proof-tree-get-proof-info
   proof-tree-find-begin-of-unfinished-proof
     'coq-find-begin-of-unfinished-proof)

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

   proof-shell-interactive-prompt-regexp "TcDebug "

   ;; ****** is added at the end of warnings in emacs mode, this is temporary I
   ;;        want xml like tags, and I want them removed before warning display.
   ;; I want the same for errors -> pgip

   proof-shell-eager-annotation-end coq-shell-eager-annotation-end ; done
   proof-shell-annotated-prompt-regexp coq-shell-prompt-pattern
   proof-shell-result-start "\372 Pbp result \373"
   proof-shell-result-end "\372 End Pbp result \373"

;   proof-shell-start-goals-regexp          "^\\(?:(dependent evars:[^)]*)\\s-+\\)?[0-9]+\\(?: focused\\)? subgoals?"
   proof-shell-start-goals-regexp          "[0-9]+\\(?: focused\\)? \\(?:sub\\)?goals?"
   proof-shell-end-goals-regexp
   (if coq-hide-additional-subgoals
       (setq proof-shell-end-goals-regexp coq-end-goals-regexp-hide-subgoals)
     (setq proof-shell-end-goals-regexp coq-end-goals-regexp-show-subgoals))

   proof-shell-init-cmd coq-shell-init-cmd

   proof-no-fully-processed-buffer t

   proof-dependency-menu-system-specific coq-dependency-menu-system-specific
   proof-dependencies-system-specific coq-dependencies-system-specific
   ;; Coq has no global settings?
   ;; (proof-assistant-settings-cmd)

   proof-shell-restart-cmd coq-shell-restart-cmd
   pg-subterm-anns-use-stack t)

  ;; (holes-mode 1)  da: does the shell really need holes mode on?
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


(defun coq-goals-mode-config ()
  (setq pg-goals-change-goal "Show %s .")
  (setq pg-goals-error-regexp coq-error-regexp)
  (setq proof-goals-font-lock-keywords coq-goals-font-lock-keywords)
  (proof-goals-config-done))

(defun coq-response-config ()
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
  ":set function fo `coq-diffs'.
Set Diffs setting if Coq is running and has a version >= 8.10."
  (set symbol new-value)
  (if (proof-shell-available-p)
      (let ((cmd (coq-diffs)))
        (if (equal cmd "")
            (message "Ignore coq-diffs setting %s for Coq before 8.10"
                 (symbol-name coq-diffs))
          (proof-shell-invisible-command cmd)))))

(defcustom coq-diffs 'off
  "Controls Coq Diffs option"
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
  "Return the evar printing command for CMD as action list item."
  (proof-shell-action-list-item
   (concat cmd " Printing Dependent Evars Line.")
   'proof-done-invisible
   (list 'invisible 'no-response-display)))

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

(defconst module-kinds-table
  '(("Section" 0) ("Module" 1) ("Module Type" 2) ("Declare Module" 3))
  "Enumerates the different kinds of modules.")

(defconst modtype-kinds-table
  '(("" 1) (":" 2) ("<:" 3))
  "Enumerates the different kinds of type information for modules.")

(defun coq-postfix-.v-p (s)
  (string-match-p "\\.v\\'" s))

(defun coq-directories-files (l)
  (let* ((file-list-list (mapcar #'directory-files l))
         (file-list (apply #'append file-list-list))
         (filtered-list (cl-remove-if-not #'coq-postfix-.v-p file-list)))
  filtered-list))

(defun coq-remove-dot-v-extension (s)
  (substring s 0 -2))

(defun coq-load-path-to-paths (ldpth)
  (if (listp ldpth) (car ldpth) ldpth))

(defun coq-build-accessible-modules-list ()
  (let* ((pth (or coq-load-path '(".")))
         (cleanpth (mapcar #'coq-load-path-to-paths pth))
         (existingpth (cl-remove-if-not #'file-exists-p cleanpth))
         (file-list (coq-directories-files existingpth)))
    (mapcar #'coq-remove-dot-v-extension file-list)))

(defun coq-insert-section-or-module ()
  "Insert a module or a section after asking right questions."
  (interactive)
  (let*
      ((mods (completing-read "Kind of module (TAB to see list): "
                              module-kinds-table))
       (s (read-string  "Name: "))
       (typkind (if (string-equal mods "Section")
                    "" ;; if not a section
                  (completing-read "Kind of type (optional, TAB to see list): "
                                   modtype-kinds-table)))
       (p (point)))
    (if (string-equal typkind "")
        (progn
          (insert mods " " s ".\n#\nEnd " s ".")
          (holes-replace-string-by-holes-backward p)
          (goto-char p))
      (insert mods " " s " " typkind " #.\n#\nEnd " s ".")
      (holes-replace-string-by-holes-backward p)
      (goto-char p)
      (holes-set-point-next-hole-destroy))))

(defconst reqkinds-kinds-table
  '(("Require Import") ("Require Export") ("Require") ("Import"))
  "Enumerates the different kinds of requiring a module.")

(defun coq-insert-requires ()
  "Insert requires to modules, iteratively."
  (interactive)
  (let* ((s)
         (reqkind
          (completing-read
           "Command (TAB to see list, default Require Import) : "
           reqkinds-kinds-table nil nil nil nil "Require Import")))
    (cl-loop do
             (setq s (completing-read "Name (empty to stop) : "
                                      (coq-build-accessible-modules-list)))
             (unless (zerop (length s)) (insert (format "%s %s.\n" reqkind s)))
             while (not (string-equal s "")))))

;; TODO add module closing
(defun coq-end-Section ()
  "Ends a Coq section."
  (interactive)
  (let ((count 1)) ; The number of section already "Ended" + 1
    (let ((section
	   (save-excursion
	     (progn
	       (while (and (> count 0)
			   (search-backward-regexp
			    "Chapter\\|Section\\|End" 0 t))
		 (if (char-equal (char-after (point)) ?E)
		     (setq count (1+ count))
		   (setq count (1- count))))
	       (buffer-substring-no-properties
          (progn (beginning-of-line) (forward-word 1) (point))
          (progn (end-of-line) (point)))))))
      (insert (concat "End" section)))))


(defun coq--format-intros (output)
  "Create an “intros” or ”move” form from the OUTPUT of “Show Intros”."
  (let* ((shints1 (replace-regexp-in-string "^[0-9] \\(?:sub\\)?goal\\(.\\|\n\\|\r\\)*"  "" output))
         (shints (replace-regexp-in-string "[\r\n ]*\\'" "" shints1)))
    (if (or (string= "" shints)
            (string-match coq-error-regexp shints))
        (error "Don't know what to intro")
      (save-excursion
        (if (re-search-backward "Require.*ssreflect" nil t)
            (format "move=> %s" shints)
          (format "intros %s" shints))))))

(defun coq-insert-intros ()
  "Insert an intros or move command with names given by Show Intros.
Based on idea mentioned in Coq reference manual."
  (interactive)
  (let* ((output (proof-shell-invisible-cmd-get-result "Show Intros.")))
    (indent-region (point)
                   (progn (insert (coq--format-intros output))
                          (save-excursion
                            (insert (if coq-one-command-per-line "\n" " "))
                            (point))))
    ;; `proof-electric-terminator' moves the point in all sorts of strange
    ;; ways, so we run it last
    (let ((last-command-event ?.)) ;; Insert a dot
      (proof-electric-terminator))))

(defvar coq-keywords-accepting-as-regex (regexp-opt '("induction" "destruct" "inversion" "injection")))

;; destruct foo., where foo is a name.
(defvar coq-auto-as-hack-hyp-name-regex
  (concat "\\(" "induction\\|destruct" "\\)\\s-+\\(" coq-id-shy "\\)\\s-*\\.")
  "Regexp of commands needing a hack to generate correct \"as\" close.
tacitcs like destruct and induction reuse hypothesis names by
default, which makes the detection of new hypothesis incorrect.
the hack consists in adding the \"!\" modifier on the argument
destructed, so that it is left in the goal and the name cannot be
reused.  We also had a \"clear\" at the end of the tactic so that
the whole tactic behaves correctly.
Warning: this makes the error messages (and location) wrong.")

(defun coq-hack-cmd-for-infoH (s)
  "return the tactic S hacked with infoH tactical."
  (cond
   ((string-match ";" s) s) ;; composed tactic cannot be treated
   ((string-match coq-auto-as-hack-hyp-name-regex s)
    (concat "infoH " (match-string 1 s) " (" (match-string 2 s) ")."))
   ((string-match coq-keywords-accepting-as-regex s)
    (concat "infoH " s))
   (t (concat "infoH " s))))

;; Point supposed to be at the end of locked region, that is
;; (proof-assert-next-command-interactive) has just finished
(defun coq-tactic-already-has-an-as-close(s)
  "Return t if the last tactic of locked region contains an \"as\" close."
  (save-excursion (string-match "\\<as\\>" s)))

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
  "Whether and how proofgeneral should insert \"Proof using\" suggestions.
Suggestions are emitted by Coq at Qed time. The possible values
of this variable are:

- 'always: always insert the suggested annotation

- 'highlight (default value): highlight the Proof command and
  have a contextual menu for insertion (or M-x
  coq-insert-suggested-dependency when point is on the proof
  considered)

- 'ask: asks the user each time. If yes then do as 'always, else
  do as 'highlight

- 'never: ignore completely the suggestions.

Remarks and limitations:
- do not support nested proofs.
- always use the *first* annotation suggested by coq.
- the proof is not replayed, which is consistent with the fact
  that pg currently do not deal with async proofs.
- if there is already a \"Proof using\" annotation, then either it
  is correct and nothing happens, or it is incorrect and coq
  generates an error. PG won't try to replace the erroneous
  annotation in this case, but you can always go back, remove it
  by hand, and let pg insert the good one.
- instead of the menu you can do M-x coq-insert-suggested-dependency
  when point is on the proof considered.
"
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
script. Actually the only possible content iafter Proof is a
\"with annotation\".
The substring matched numbered 1 must start at the possible
insertion point for the \"using\" annotation. ")


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
  "look for a \"Proof\" command in span SPAN.

Return nil if not found. Set point to the end of \"Proof\"
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


(defun coq-insert-as-in-next-command ()
  (interactive)
  (save-excursion
    (goto-char (proof-unprocessed-begin))
    (coq-find-real-start)
    (let* ((pt (point))
           (_ (coq-script-parse-cmdend-forward))
           (cmd (buffer-substring pt (point)))
           (newcmd (if (coq-tactic-already-has-an-as-close cmd)
                       nil
                     (coq-hack-cmd-for-infoH cmd))))
      (when newcmd ; FIXME: we stop if as already there, replace it instead?
        (proof-shell-invisible-command newcmd t)
        (let* ((str (string-match "<infoH>\\([^<]*\\)</infoH>"
                                  ;; proof-shell-last-response-output would be
                                  ;; smaller/faster but since this message is output
                                  ;; *before* resulting goals, it is not detected as
                                  ;; a response message.
                                  proof-shell-last-output))
               (substr  (or (and str (match-string 1 proof-shell-last-output)) ""))
               ;; emptysubstr = t if substr is empty or contains only spaces and |
               (emptysubstr (and (string-match "\\(\\s-\\||\\)*" substr)
                                 (eq (length substr) (length (match-string 0 substr)))))) ; idem
          (unless (or emptysubstr (coq-tactic-already-has-an-as-close newcmd)) ;; FIXME
            (save-excursion
              ;; TODO: look for eqn:XX and go before it.
              ;; Go just before the last "."
              (goto-char (proof-unprocessed-begin))
              (coq-script-parse-cmdend-forward)
              (coq-script-parse-cmdend-backward)
              (insert (concat " as [" substr "]")))))))))

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



(defun coq-insert-match ()
  "Insert a match expression from a type name by Show Match.
Based on idea mentioned in Coq reference manual.
Also insert holes at insertion positions."
  (interactive)
  (proof-shell-ready-prover)
  (let* ((cmd))
    (setq cmd (read-string "Build match for type: "))
    (let* ((thematch
           (proof-shell-invisible-cmd-get-result (concat "Show Match " cmd ".")))
           (match (replace-regexp-in-string "=> *\n" "=> #\n" thematch)))
      ;; if error, it will be displayed in response buffer (see def of
      ;; proof-shell-invisible-cmd-get-result), otherwise:
      (unless (proof-string-match coq-error-regexp match)
        (let ((start (point)))
          (insert match)
          (indent-region start (point) nil)
          (let ((n (holes-replace-string-by-holes-backward start)))
            (pcase n
	      (0 nil)                   ; no hole, stay here.
	      (1
	       (goto-char start)
	       (holes-set-point-next-hole-destroy)) ; if only one hole, go to it.
	      (_
	       (goto-char start)
	       (message
                (substitute-command-keys
                 "\\[holes-set-point-next-hole-destroy] to jump to active hole.  \\[holes-short-doc] to see holes doc."))))))))))

(defun coq-insert-solve-tactic ()
  "Ask for a closing tactic name, with completion, and insert at point.
Completion is on a quasi-exhaustive list of Coq closing tactics."
  (interactive)
  (coq-insert-from-db coq-solve-tactics-db "Closing tactic"))

(defun coq-insert-tactic ()
  "Insert a tactic name at point, with completion.
Questions may be asked to the user to select the tactic."
  (interactive)
  (coq-insert-from-db coq-tactics-db "Tactic"))

(defun coq-insert-tactical ()
  "Ask for a closing tactic name, with completion, and insert at point.
Completion is on a quasi-exhaustive list of Coq tacticals."
  (interactive)
  (coq-insert-from-db coq-tacticals-db "Tactical"))

(defun coq-insert-command ()
  "Ask for a command name, with completion, and insert it at point."
  (interactive)
  (coq-insert-from-db coq-commands-db "Command"))

(defun coq-insert-term ()
  "Ask for a term kind, with completion, and insert it at point."
  (interactive)
  (coq-insert-from-db coq-terms-db "Kind of term"))


(defun coq-query (showall)
  "Ask for a query, with completion, and send to Coq."
  (interactive "P")
  (let ((q (coq-build-command-from-db coq-queries-commands-db "which Query?")))
    (if showall
        (coq-command-with-set-unset
         "Set Printing All" q "Unset Printing All" nil "Test Printing All")
      (proof-shell-invisible-command q))))

(defun coq-Search ()
  (interactive)
  (coq-ask-do
   ;; TODO: use [Add Search Blacklist "foo"] to exclude optionaly some patterns:
   ;;  "_ind" "_rec" "R_" "_equation"
   "Search (ex: \"eq_\" eq -bool)"
   "Search"
   nil nil t)
  (when coq-highlight-hyps-cited-in-response
    (coq-highlight-hyps-cited-in-response)
    (message "M-x coq-unhighlight-selected-hyps to remove highlighting, [Coq/Settings/Search Blacklist] to change blacklisting."))
  (unless coq-highlight-hyps-cited-in-response
    (message "[Coq/Settings/Search Blacklist] to change blacklisting.")))


;; Insertion commands
(define-key coq-keymap [(control ?i)]  #'coq-insert-intros)
(define-key coq-keymap [(control ?m)]  #'coq-insert-match)
(define-key coq-keymap [(control ?\()] #'coq-insert-section-or-module)
(define-key coq-keymap [(control ?\))] #'coq-end-Section)
(define-key coq-keymap [(control ?t)]  #'coq-insert-tactic)
(define-key coq-keymap [?t]            #'coq-insert-tactical)
(define-key coq-keymap [?!]            #'coq-insert-solve-tactic) ; will work in tty
(define-key coq-keymap [(control ?\s)] #'coq-insert-term)
(define-key coq-keymap [(control return)] #'coq-insert-command)
(define-key coq-keymap [(control ?q)]  #'coq-query)
(define-key coq-keymap [(control ?r)]  #'coq-insert-requires)
; [ for "as [xxx]" is easy to remember, ccontrol-[ would be better but hard to type on french keyboards
; anyway company-coq should provide an "as!<TAB>".
(define-key coq-keymap [(?\[)]         #'coq-insert-as-in-next-command) ;; not for goal/response buffer?

; Query commands
(define-key coq-keymap [(control ?s)]  #'coq-Show)
(define-key coq-keymap [?r]            #'proof-store-response-win)
(define-key coq-keymap [?g]            #'proof-store-goals-win)
(define-key coq-keymap [(control ?o)]  #'coq-SearchIsos)
(define-key coq-keymap [(control ?p)]  #'coq-Print)
(define-key coq-keymap [(control ?b)]  #'coq-About)
(define-key coq-keymap [(control ?a)]  #'coq-Search)
(define-key coq-keymap [(control ?c)]  #'coq-Check)
(define-key coq-keymap [?h]            #'coq-PrintHint)
(define-key coq-keymap [(control ?l)]  #'coq-LocateConstant)
(define-key coq-keymap [(control ?n)]  #'coq-LocateNotation)
(define-key coq-keymap [(control ?w)]  #'coq-ask-adapt-printing-width-and-show)
(define-key coq-keymap [(control ?9)]  #'coq-set-printing-parentheses)
(define-key coq-keymap [(control ?0)]  #'coq-unset-printing-parentheses)
(define-key coq-keymap [(?N)]          #'coq-set-printing-notations)
(define-key coq-keymap [(?n)]          #'coq-unset-printing-notations)

;(proof-eval-when-ready-for-assistant
; (define-key ??? [(control c) (control a)] (proof-ass keymap)))

;(proof-eval-when-ready-for-assistant
; (define-key ??? [(control c) (control a)] (proof-ass keymap)))

(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?c)] #'coq-Check)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?p)] #'coq-Print)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?o)] #'coq-SearchIsos)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?b)] #'coq-About)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?a)] #'coq-Search)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?s)] #'coq-Show)
(define-key coq-goals-mode-map [(control ?c)(control ?a)?r] #'proof-store-response-win)
(define-key coq-goals-mode-map [(control ?c)(control ?a)?g] #'proof-store-goals-win)
(define-key coq-goals-mode-map [(control ?c)(control ?a)?h] #'coq-PrintHint)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?q)] #'coq-query)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?w)] #'coq-ask-adapt-printing-width-and-show)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?l)] #'coq-LocateConstant)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?n)] #'coq-LocateNotation)
;; specific to goals buffer: (un)foldinng and (un)highlighting shortcuts
(define-key coq-goals-mode-map [?f] #'coq-toggle-fold-hyp-at-point)
(define-key coq-goals-mode-map [?F] #'coq-unfold-hyps)
(define-key coq-goals-mode-map [?h] #'coq-toggle-highlight-hyp-at-point)
(define-key coq-goals-mode-map [?H] #'coq-unhighlight-selected-hyps)


(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?c)] #'coq-Check)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?p)] #'coq-Print)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?o)] #'coq-SearchIsos)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?b)] #'coq-About)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?a)] #'coq-Search)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?s)] #'coq-Show)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?r)] #'proof-store-response-win)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?g)] #'proof-store-goals-win)
(define-key coq-response-mode-map [(control ?c)(control ?a)?h] #'coq-PrintHint)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?q)] #'coq-query)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?w)] #'coq-ask-adapt-printing-width-and-show)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?l)] #'coq-LocateConstant)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?n)] #'coq-LocateNotation)

(when coq-remap-mouse-1
  (define-key proof-mode-map [(control down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-mode-map [(shift down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-mode-map [(control mouse-1)] #'ignore)
  (define-key proof-mode-map [(shift mouse-1)] #'ignore)
  (define-key proof-mode-map [(control shift down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-mode-map [(control shift mouse-1)] #'ignore)

  (define-key proof-response-mode-map [(control down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-response-mode-map [(shift down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-response-mode-map [(control mouse-1)] #'ignore)
  (define-key proof-response-mode-map [(shift mouse-1)] #'ignore)
  (define-key proof-response-mode-map [(control shift down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-response-mode-map [(control shift mouse-1)] #'ignore)

  (define-key proof-goals-mode-map [(control down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-goals-mode-map [(shift down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-goals-mode-map [(control mouse-1)] #'ignore)
  (define-key proof-goals-mode-map [(shift mouse-1)] #'ignore)
  (define-key proof-goals-mode-map [(control shift down-mouse-1)] #'coq-id-under-mouse-query)
  (define-key proof-goals-mode-map [(control shift mouse-1)] #'ignore))



;; Default binding on hypothesis names: clicking on the name of an hyp with
;; button 3 folds it. Click on it with button 2 copies the names at current
;; point.
(when coq-hypname-map
  (define-key coq-hypname-map [(mouse-3)] #'coq-toggle-fold-hyp-at-mouse)
  (define-key coq-hypcross-map [(return)] #'coq-toggle-fold-hyp-at-point)
  (define-key coq-hypname-map [(mouse-2)] #'coq-insert-at-point-hyp-at-mouse))

;; Default binding: clicking on the cross to folds/unfold hyp.
;; Click on it with button 2 copies the names at current point.
(when coq-hypname-map
  (define-key coq-hypcross-map [(mouse-1)] #'coq-toggle-fold-hyp-at-mouse)
  (define-key coq-hypcross-map [(return)]  #'coq-toggle-fold-hyp-at-point)
  (define-key coq-hypcross-map [(mouse-2)] #'coq-insert-at-point-hyp-at-mouse))
;; Ddefault binding: clicking on a hidden hyp with button 3 unfolds it, with
;; button 2 it copies hyp name at current point.
(when coq-hidden-hyp-map
  (define-key coq-hidden-hyp-map [(mouse-1)] #'coq-toggle-fold-hyp-at-mouse)
  (define-key coq-hypcross-map   [(return)]  #'coq-toggle-fold-hyp-at-point)
  (define-key coq-hidden-hyp-map [(mouse-2)] #'coq-insert-at-point-hyp-at-mouse))

;;;;;;;;;;;;;;;;;;;;;;;;
;; error handling
;;;;;;;;;;;;;;;;;;;;;;;;


(defvar last-coq-error-location nil
  "Last error from `coq-get-last-error-location' and `coq-highlight-error'.")

(defvar coq--error-location-regexp
  "^Toplevel input, characters \\(?1:[0-9]+\\)-\\(?2:[0-9]+\\):\\(\n> [^\n]*\\)*\n"
  "A regexp to search the header of coq error locations.")

;; I don't use proof-shell-last-output here since it is not always set to the
;; really last output (specially when a *tactic* gives an error) instead I go
;; directly to the response buffer. This allows also to clean the response
;; buffer (better to only scroll it?)
(defun coq-get-last-error-location (&optional parseresp clean)
  "Return location information on last error sent by coq.

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

Important: Coq gives char positions in bytes instead of chars.
"
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
                ;; user usually expect the point to move to the error location
                (goto-char beg))))))))

(defun coq-highlight-error-hook ()
  (coq-highlight-error t t))

(add-hook 'proof-shell-handle-error-or-interrupt-hook #'coq-highlight-error-hook t)


;;
;; Scroll response buffer to maximize display of first goal
;;

(defun coq-first-word-before (reg)
  "Get the word before first string matching REG in current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward reg nil t)
    (goto-char (match-beginning 0))
    (backward-word 1)
    (buffer-substring (point)
                      (progn (forward-word 1) (point)))))

(defun coq-get-from-to-paren (reg)
  "Get the word after first string matching REG in current buffer."
  (save-excursion
    (goto-char (point-min))
    (if (null (re-search-forward reg nil t)) ""
      (goto-char (match-end 0))
      (let ((p (point)))
        (if (null (re-search-forward ")" nil t))
            ""
          (goto-char (match-beginning 0))
          (buffer-substring p (point)))))))



(defun coq-show-first-goal ()
  "Scroll the goal buffer so that the first goal is visible.
First goal is displayed on the bottom of its window, maximizing the
number of hypothesis displayed, without hiding the goal"
  (interactive)
  ;; CPC 2015-12-31: Added the check below: if the command that caused this
  ;; call was silent, we shouldn't touch the goals buffer.  See GitHub issues
  ;; https://github.com/cpitclaudel/company-coq/issues/32 and
  ;; https://github.com/cpitclaudel/company-coq/issues/8.
  (unless (memq 'no-goals-display proof-shell-delayed-output-flags)
    (let ((pg-frame (car (coq-find-threeb-frames)))) ; selecting the good frame
      (with-selected-frame (or pg-frame (window-frame (selected-window)))
        ;; prefer current frame
        (let ((goal-win (or (get-buffer-window proof-goals-buffer) (get-buffer-window proof-goals-buffer t))))
          (if goal-win
              (with-selected-window goal-win
                ;; find snd goal or buffer end, if not found this goes to the
                ;; end of buffer
                (search-forward-regexp "\\(?:sub\\)?goal 2\\|\\*\\*\\* Unfocused goals:\\|\\'")
                (beginning-of-line)
                ;; find something backward else than a space: bottom of concl
                (ignore-errors (search-backward-regexp "\\S-"))
                (recenter (- 1)) ; put bot of concl at bottom of window
                (beginning-of-line)
                ;; if the top of concl is hidden we may want to show it instead
                ;; of bottom of concl
                (when (and coq-prefer-top-of-conclusion
                         ;; return nil if === is not visible
                         (not (save-excursion (re-search-backward "========" (window-start) t))))
                  (re-search-backward "========" nil t)
                  (recenter 0))
                (beginning-of-line))))))))

(defvar coq-modeline-string2 ")")
(defvar coq-modeline-string1 ")")
(defvar coq-modeline-string0 " Script(")
(defun coq-build-subgoals-string (n s)
  (concat coq-modeline-string0 (int-to-string n)
          "-" s
          (if (> n 1) coq-modeline-string2
            coq-modeline-string1)))

(defun coq-update-minor-mode-alist ()
  "Modify `minor-mode-alist' to display the number of subgoals in the modeline."
  (when (and proof-goals-buffer proof-script-buffer)
    (let ((nbgoals (with-current-buffer proof-goals-buffer
                     (string-to-number (coq-first-word-before "focused\\|\\(?:sub\\)?goal"))))
          (nbunfocused (with-current-buffer proof-goals-buffer
                         (coq-get-from-to-paren "unfocused: "))))
      (with-current-buffer proof-script-buffer
        (let ((toclean (assq 'proof-active-buffer-fake-minor-mode minor-mode-alist)))
          (while toclean ;; clean minor-mode-alist
            (setq minor-mode-alist (remove toclean minor-mode-alist))
            (setq toclean (assq 'proof-active-buffer-fake-minor-mode minor-mode-alist)))
          (setq minor-mode-alist
                (append (list (list 'proof-active-buffer-fake-minor-mode
                                    (coq-build-subgoals-string nbgoals nbunfocused)))
                        minor-mode-alist)))))))





;; This hook must be added before coq-optimise-resp-windows, in order to be evaluated
;; *after* windows resizing.
(add-hook 'proof-shell-handle-delayed-output-hook
	  #'coq-show-first-goal)
(add-hook 'proof-shell-handle-delayed-output-hook
	  #'coq-update-minor-mode-alist)
(add-hook 'proof-shell-handle-delayed-output-hook
          (lambda ()
            (if (proof-string-match coq-shell-proof-completed-regexp
                                    proof-shell-last-output)
                (proof-clean-buffer proof-goals-buffer))))

(add-hook 'proof-shell-handle-delayed-output-hook
	  (lambda ()
            (setq coq-hyps-positions (coq-detect-hyps-in-goals))
            (when coq-highlight-hyps-sticky (coq-highlight-selected-hyps))
            (coq-fold-hyps)
            ))



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
;; Trying to have double hit on colon behave like electric terminator. The "."
;; is used for records and modules qualified notatiohns, so electric terminator
;; is not pertinent.

;; TODO: make this a minor mode with something in the modeline, like in
;; pg-user.el for electric-terminator.
;; TODO: Have the same for other commands, but with insertion at all.

(defcustom coq-double-hit-enable nil
  "* Experimental: Whether or not double hit should be enabled in coq mode.
A double hit is performed by pressing twice a key quickly.  If
this variable is not nil, then 1) it means that electric
terminator is off and 2) a double hit on the terminator act as
the usual electric terminator.  See `proof-electric-terminator'."
  :type 'boolean
  :set #'proof-set-value
  :group 'proof-user-options)


(defvar coq-double-hit-hot-key "."
  "The key used for double hit electric terminator.
By default this is the coq terminator \".\" key.
For example one can do this:

\(setq coq-double-hit-hot-key (kbd \";\"))

to use semi-colon instead (on French keyboard, it is the same key as
\".\" but without shift.")

(defvar coq-double-hit-hot-keybinding nil
  "The keybinding that was erased by double hit terminator enabling.
It will be restored if double hit terminator is toggle off.")

;; We redefine the keybinding when we go in and out of double hit mode, even if
;; in principle coq-terminator-insert is compatible with
;; proof-electric-terminator. This may be overprudent but I suspect that
(defun coq-double-hit-enable ()
  "Disables electric terminator since double hit is a replacement.
This function is called by `proof-set-value' on `coq-double-hit-enable'."
  (when (and coq-double-hit-enable proof-electric-terminator-enable)
    (proof-electric-terminator-toggle 0))
  ;; this part switch between bindings of coq-double-hit-hot-key: the nominal
  ;; one and coq-terminator-insert
;  (if (not coq-double-hit-enable)
;      (define-key coq-mode-map (kbd coq-double-hit-hot-key) coq-double-hit-hot-keybinding)
;    (setq coq-double-hit-hot-keybinding (key-binding coq-double-hit-hot-key))
;    (define-key coq-mode-map (kbd coq-double-hit-hot-key) 'coq-terminator-insert))
  )



(proof-deftoggle coq-double-hit-enable coq-double-hit-toggle)

(defadvice proof-electric-terminator-enable (after coq-unset-double-hit-advice)
  "Disable double hit terminator since electric terminator is a replacement.
This is an advice to pg `proof-electric-terminator-enable' function."
  (when (and coq-double-hit-enable proof-electric-terminator-enable)
    (coq-double-hit-toggle 0)
    (message "Hit M-1 . to enter a real \".\".")))

(ad-activate 'proof-electric-terminator-enable)

(defvar coq-double-hit-delay 0.25
  "The maximum delay between the two hit of a double hit in coq/proofgeneral.")

(defvar coq-double-hit-timer nil
  "The timer used to watch for double hits.")

(defvar coq-double-hit-hot nil
  "The variable telling that a double hit is still possible.")



(defun coq-unset-double-hit-hot ()
  "Disable timer `coq-double-hit-timer' and set it to nil.
Shut off the current double hit if any.  This function is supposed to
be called at double hit timeout."
  (when coq-double-hit-timer (cancel-timer coq-double-hit-timer))
  (setq coq-double-hit-hot nil)
  (setq coq-double-hit-timer nil))

(defun coq-colon-self-insert ()
  "Detect a double hit and act as electric terminator if detected.
Starts a timer for a double hit otherwise."
  (interactive)
  (if (and coq-double-hit-hot
           (not (proof-inside-comment (point)))
           (not (proof-inside-string (point))))
      (progn (coq-unset-double-hit-hot)
             (delete-char -1) ; remove previously typed char
             (proof-assert-electric-terminator)); insert the terminator
    (self-insert-command 1)
    (setq coq-double-hit-hot t)
    (setq coq-double-hit-timer
          (run-with-timer coq-double-hit-delay
                          nil #'coq-unset-double-hit-hot))))

(defun coq-terminator-insert (&optional count)
  "A wrapper on `proof-electric-terminator'.
Used to accept double hits instead if enabled.
If by accident option `proof-electric-terminator-enable' and option
`coq-double-hit-enable' are non-nil at the same time, this gives
priority to the former."
  (interactive)
  (if (and (not proof-electric-terminator-enable)
           coq-double-hit-enable (null count))
      (coq-colon-self-insert)
    ;; otherwise call this, which checks proof-electric-terminator-enable
    (proof-electric-terminator count)))

(put 'coq-terminator-insert 'delete-selection t)


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
  "If t kill coq when the opam switch changes (requires `opam-switch-mode').
When `opam-switch-mode' is loaded and the user changes the opam switch
through `opam-switch-mode' then this option controls whether the coq
background process (the proof shell) is killed such that the next
assert command starts a new proof shell, probably using a
different coq version from a different opam switch.

See https://github.com/ProofGeneral/opam-switch-mode for `opam-switch-mode'"
  :type 'boolean
  :group 'coq)


(defun coq-kill-proof-shell-on-opam-switch ()
  "Kill proof shell when the opam switch changes (requires `opam-switch-mode').
This function is for the `opam-switch-mode' hook
`opam-switch-change-opam-switch-hook', which runs when the user
changes the opam switch through `opam-switch-mode'. If
`coq-kill-coq-on-opam-switch' is t, then the proof shell is
killed such that the next assert starts a new proof shell, using
coq from the new opam switch."
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
