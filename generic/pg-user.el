;;; pg-user.el --- User level commands for Proof General  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2019  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Author:     David Aspinall and others

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This file defines some user-level commands.  Most of them
;; are script-based operations.  Exported user-level commands
;; are defined here as autoloads to avoid circular requires.

;;; Code:

(require 'span)
(require 'scomint)

(require 'proof-script)	        ; we build on proof-script

;; For seq-group-by inside `proof-check-report'. This is apparently
;; not automatically loaded in Emacs 26.3. In a real PG session it is
;; apparently present, but not if `proof-check-proof' is invoked from
;; the shell as described in the user manual.
;; XXX Delete this when support for Emacs 26 is dropped (hopefully in Q4/2025).
(require 'seq)

(eval-when-compile (require 'cl-lib))   ;cl-decf
(defvar which-func-modes)               ; Defined by which-func.

(declare-function proof-segment-up-to "proof-script")
(declare-function proof-interrupt-process "proof-shell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities: moving point in proof script buffer
;;

;; First: two commands for moving forwards in proof scripts.  Moving
;; forward for a "new" command may insert spaces or new lines.  Moving
;; forward for the "next" command does not.

;;;###autoload
(defun proof-script-new-command-advance ()
  "Move point to a nice position for a new command, possibly inserting spaces.
Assumes that point is at the end of a command.
No effect if `proof-next-command-insert-space' is nil."
  (interactive)
  (when proof-next-command-insert-space
    (let (sps)
      (if (and (proof-next-command-new-line)
	       (setq sps (skip-chars-forward " \t"))
	       ;; don't break existing lines
	       (eolp))
	  (progn (newline)
		 (unless proof-next-command-on-new-line
		   (indent-relative))))
      (unless (proof-next-command-new-line)
	;; Multiple commands per line: skip spaces at point, and insert
	;; the 1/0 number of spaces that were skipped in front of point
	;; (at least one).  This has the pleasing effect that the spacing
	;; policy of the current line is copied: e.g.  <command>;
	;; <command>; Tab columns don't work properly, however.
	(let ((newspace (max (or sps 1) (skip-chars-forward " \t")))
	      (p (point)))
	  (insert-char ?\040 newspace)
	  (goto-char p))))))


(defun proof-maybe-follow-locked-end (&optional pos)
  "Move point according to `proof-follow-mode'.
If optional POS is set, use that position, else `proof-queue-or-locked-end'.
Assumes script buffer is current."
  (unless (or proof-autosend-running (eq proof-follow-mode 'ignore))
    (let ((dest (or pos (proof-queue-or-locked-end))))
      (cond
       ((eq proof-follow-mode 'locked)
	(goto-char dest)
	(or pos (proof-script-next-command-advance)))
       ((eq proof-follow-mode 'follow)
	(unless (pos-visible-in-window-p dest)
	  (let ((win (get-buffer-window (current-buffer) t)))
	    (if win
		(set-window-point win dest)))))
       ((and (eq proof-follow-mode 'followdown)
	     (> dest (point)))
	(goto-char dest))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Further movement commands
;;

(defun proof-goto-command-start ()
  "Move point to start of current (or final) command of the script."
  (interactive)
  (let* ((cmd (span-at (point) 'type))
	 (start (if cmd (span-start cmd))))
    (if start
	(progn
	  ;; BUG: only works for unclosed proofs.
	  (goto-char start))
      (let ((semis (nth 1 (proof-segment-up-to (point) t))))
	(if (eq 'unclosed-comment (car-safe semis))
	    (setq semis (cdr-safe semis)))
	(if (nth 2 semis) ; fetch end point of previous command
	      (goto-char (nth 2 semis))
	  ;; no previous command: just next to end of locked
	  (goto-char (proof-unprocessed-begin)))))
    ;; Oddities of this function: if we're beyond the last proof
    ;; command, it jumps back to the last command.  Could alter this
    ;; by spotting that command end of last of semis is before
    ;; point.  Also, behaviour with comments is different depending
    ;; on whether locked or not.
    (skip-chars-forward " \t\n")))

(defun proof-goto-command-end ()
  "Set point to end of command at point."
  (interactive)
  (let ((cmd (span-at (point) 'type)))
    (if cmd
	(goto-char (span-end cmd))
      (let ((semis (save-excursion
		     (proof-segment-up-to-using-cache (point)))))
	(if semis
	    (progn
	      (goto-char (nth 2 (car semis)))
	      (skip-chars-backward " \t\n")
	      (unless (eq (point) (point-min))
		(backward-char))))))))

(defun proof-forward-command (&optional num)
  "Move forward to the start of the next proof region.
If called interactively, NUM is given by the prefix argument."
  (interactive "p")
  (skip-chars-forward " \t\n")
  (let* ((span  (or (span-at (point) 'type)
		    (and (skip-chars-backward " \t\n")
			 (> (point) (point-min))
			 (span-at (1- (point)) 'type))))
	 (nextspan (and span (pg-numth-span-higher-or-lower
			      (pg-control-span-of span) num 'noerr))))
    (cond
     ((and nextspan (> num 0))
      (goto-char (span-start nextspan))
      (skip-chars-forward " \t\n"))
     ((and nextspan (< num 0))
      (goto-char (span-end nextspan)))
     ((and span (> num 0))
      (goto-char (span-end span)))
     ((and span (< num 0))
      (goto-char (span-start span))))))

(defun proof-backward-command (&optional num)
  (interactive "p")
  (proof-forward-command (- num)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Processing commands
;;

;;;###autoload
(defun proof-goto-point (&optional raw)
  "Assert or retract to the command at current position.
Calls `proof-assert-until-point' or `proof-retract-until-point' as
appropriate.
With prefix argument RAW, the activation of the omit proofs feature
(`proof-omit-proofs-option') is temporarily toggled,
so we can chose whether to check all proofs in the asserted region,
or to merely assume them and save time."
  (interactive "P")
  (let ((proof-omit-proofs-option proof-omit-proofs-option))
    (when raw
      (setq proof-omit-proofs-option (not proof-omit-proofs-option)))
    (save-excursion
      (if (> (proof-queue-or-locked-end) (point))
	  (proof-retract-until-point)
        (if (proof-only-whitespace-to-locked-region-p)
	    (progn
	      (skip-chars-forward " \t\n")
	      (forward-char 1)))
        (proof-assert-until-point)))))

(defun proof-assert-next-command-interactive ()
  "Process until the end of the next unprocessed command after point.
If inside a comment, just process until the start of the comment."
  (interactive)
  (proof-with-script-buffer ; for toolbar/other buffers
   (save-excursion
     (goto-char (proof-queue-or-locked-end))
     (skip-chars-forward " \t\n")
     (proof-assert-until-point))
   (proof-maybe-follow-locked-end)))

;; NB: "interactive" variant merely for a simple docstring.
(defun proof-assert-until-point-interactive ()
  "Process the region from the end of the locked-region until point.
If inside a comment, just process until the start of the comment."
  (interactive)
  (proof-assert-until-point))

;;;###autoload
(defun proof-process-buffer (&optional raw)
  "Process the current (or script) buffer, and maybe move point to the end.
With prefix argument RAW, the activation of the omit proofs feature
(`proof-omit-proofs-option') is temporarily toggled,
so we can chose whether to check all proofs in the asserted region,
or to merely assume them and save time."
  (interactive "P")
  (let ((proof-omit-proofs-option proof-omit-proofs-option))
    (when raw
      (setq proof-omit-proofs-option (not proof-omit-proofs-option)))
    (proof-with-script-buffer
     (save-excursion
       (goto-char (point-max))
       (proof-assert-until-point-interactive))
     (proof-maybe-follow-locked-end))
    (when proof-fast-process-buffer
      (message "Processing buffer...")
      (proof-shell-wait)
      (message "Processing buffer...done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Undoing commands
;;

(defun proof-undo-last-successful-command ()
  "Undo last successful command at end of locked region."
  (interactive)
  (proof-undo-last-successful-command-1))

(defun proof-undo-and-delete-last-successful-command ()
  "Undo and delete last successful command at end of locked region.
Useful if you typed completely the wrong command.
Also handy for proof by pointing, in case the last proof-by-pointing
command took the proof in a direction you don't like.

Notice that the deleted command is put into the Emacs kill ring, so
you can use the usual `yank' and similar commands to retrieve the
deleted text."
  (interactive)
  (proof-undo-last-successful-command-1 'kill-region))

(defun proof-undo-last-successful-command-1 (&optional undo-action)
  "Undo last successful command at end of locked region.
If optional UNDO-ACTION is non-nil, that function is called on
the text region in the proof script after undoing."
  (interactive "P")
  (proof-with-script-buffer
   (let (lastspan)
     (save-excursion
       (unless (proof-locked-region-empty-p)
	 (if (setq lastspan (span-at-before (proof-unprocessed-begin) 'type))
	     (progn
	       (goto-char (span-start lastspan))
	       (proof-retract-until-point undo-action))
	   (error "Nothing to undo!"))))
     (if lastspan (proof-maybe-follow-locked-end
		   (span-start lastspan))))))

(defun proof-retract-buffer (&optional called-interactively)
  "Retract the current buffer, and maybe move point to the start.
Point is only moved according to `proof-follow-mode', if
CALLED-INTERACTIVELY is non-nil, which is the case for all
interactive calls."
  ;; The numeric prefix argument "p" is never nil,
  ;; see Section  "Distinguish Interactive Calls" in the Elisp manual.
  (interactive "p")
  (proof-with-script-buffer
   (save-excursion
    (goto-char (point-min))
    (proof-retract-until-point-interactive))
   (if called-interactively
       (proof-maybe-follow-locked-end (point-min)))))

(defun proof-retract-current-goal ()
  "Retract the current proof, and move point to its start."
  (interactive)
   (let
      ((span (proof-last-goal-or-goalsave)))
     (save-excursion
       (if (and span (not (eq (span-property span 'type) 'goalsave))
		(< (span-end span) (proof-unprocessed-begin)))
	   (progn
	     (goto-char (span-start span))
	     (proof-retract-until-point-interactive))
	 (error "Not proving")))
     (if span (proof-maybe-follow-locked-end (span-start span)))))


;;
;; Mouse functions
;;

(defun proof-mouse-goto-point (event)
  "Call `proof-goto-point' on the click position EVENT."
  (interactive "e")
  (proof-with-script-buffer
   (mouse-set-point event)
   (proof-goto-point)
   (proof-maybe-follow-locked-end)))




;;
;; Minibuffer non-scripting command
;;

(defvar proof-minibuffer-history nil
  "History of proof commands read from the minibuffer.")

(defun proof-minibuffer-cmd (cmd)
  "Send CMD to proof assistant.  Interactively, read from minibuffer.
The command isn't added to the locked region.

If a prefix arg is given and there is a selected region, that is
pasted into the command.  This is handy for copying terms, etc from
the script.

If `proof-strict-state-preserving' is set, and `proof-state-preserving-p'
is configured, then the latter is used as a check that the command
will be safe to execute, in other words, that it won't ruin
synchronization.  If when applied to the command it returns false,
then an error message is given.

WARNING: this command risks spoiling synchronization if the test
`proof-state-preserving-p' is not configured, if it is
only an approximate test, or if `proof-strict-state-preserving'
is off (nil)."
  (interactive
   (list (read-string "Command: "
		      (if (and current-prefix-arg (region-active-p))
			  (replace-regexp-in-string
			   "[ \t\n]+" " "
			   (buffer-substring (region-beginning) (region-end))))
		      'proof-minibuffer-history)))
  (if (and proof-strict-state-preserving
	   proof-state-preserving-p
	   (not (funcall proof-state-preserving-p cmd)))
      (error "Command is not state preserving, I won't execute it!"))
  (proof-shell-invisible-command cmd))


;;
;; Frobbing locked end
;;

;; In fact, it's so risky, we'll disable it by default
(put 'proof-frob-locked-end 'disabled t)

(defun proof-frob-locked-end ()
  "Move the end of the locked region backwards to regain synchronization.
Only for use by consenting adults.

This command can be used to repair synchronization in case something
goes wrong and you want to tell Proof General that the proof assistant
has processed less of your script than Proof General thinks.

You should only use it to move the locked region to the end of
a proof command."
  (interactive)
  (cond
   (proof-shell-busy
    (error "You can't use this command while %s is busy!" proof-assistant))
   ((not (eq (current-buffer) proof-script-buffer))
    (error "Must be in the active scripting buffer"))
   (t (proof-set-locked-end (point))
      (span-delete-spans (proof-unprocessed-begin) (point-max) 'type))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Non-scripting proof assistant commands.
;;;

;; These are based on defcustom'd settings so that users may
;; re-configure the system to their liking.


;;
;; Helper macros and functions
;;

;; See put expression at end to give this indentation like while form
(defmacro proof-if-setting-configured (var &rest body)
  "Give error if a configuration setting VAR is unset, otherwise eval BODY."
  `(if ,var
       (progn ,@body)
     (error "Proof General not configured for this: set %s"
	    ,(symbol-name var))))

;;;###autoload
(defmacro proof-define-assistant-command (fn doc cmdvar &optional body)
  "Define FN (docstring DOC): check if CMDVAR is set, then send BODY to prover.
BODY defaults to CMDVAR, a variable."
  `(defun ,fn ()
     ,(concat doc
	      (concat "\nIssues a command to the assistant based on "
		      (symbol-name cmdvar) ".")
		"")
     (interactive)
     (proof-if-setting-configured ,cmdvar
       (proof-shell-invisible-command ,(or body cmdvar)))))

;;;###autoload
(defmacro proof-define-assistant-command-witharg (fn doc cmdvar prompt &rest body)
  "Define FN (arg) with DOC: check CMDVAR is set, PROMPT a string and eval BODY.
The BODY can contain occurrences of arg.
CMDVAR is a variable holding a function or string.  Automatically has history."
  `(progn
     (defvar ,(intern (concat (symbol-name fn) "-history")) nil
       ,(concat "History of arguments for " (symbol-name fn) "."))
     (defun ,fn (arg)
     ,(concat doc "\nIssues a command based on ARG to the assistant, using "
	      (symbol-name cmdvar) ".\n"
	      "The user is prompted for an argument.")
      (interactive
       (proof-if-setting-configured ,cmdvar
	   (if (stringp ,cmdvar)
	       (list (format ,cmdvar
			 (read-string
			   ,(concat prompt ": ") ""
			   ,(intern (concat (symbol-name fn) "-history")))))
	     (funcall ,cmdvar))))
       ,@body)))

(defun proof-issue-new-command (cmd)
  "Insert CMD into the script buffer and issue it to the proof assistant.
If point is in the locked region, move to the end of it first.
Start up the proof assistant if necessary."
  (proof-with-script-buffer
   (if (proof-shell-live-buffer)
       (if (proof-in-locked-region-p)
	   (proof-goto-end-of-locked t)))
   (proof-script-new-command-advance)
   (insert cmd)
   (proof-assert-until-point-interactive)
   (proof-script-new-command-advance)))

;;
;; Commands which do not require a prompt and send an invisible
;; command.
;;

(proof-define-assistant-command proof-prf
  "Show the current proof state."
  proof-showproof-command
  (progn
    (pg-goals-buffers-hint)
    proof-showproof-command))

(proof-define-assistant-command proof-ctxt
  "Show the current context."
  proof-context-command)

(proof-define-assistant-command proof-help
  "Show a help or information message from the proof assistant.
Typically, a list of syntax of commands available."
  proof-info-command)

(proof-define-assistant-command proof-cd
  "Change directory to the default directory for the current buffer."
  proof-shell-cd-cmd
  (proof-format-filename proof-shell-cd-cmd
	  default-directory))

(defun proof-cd-sync ()
  "If `proof-shell-cd-cmd' is set, do `proof-cd' and wait for prover ready.
This is intended as a value for `proof-activate-scripting-hook'"
  ;; The hook is set in proof-mode before proof-shell-cd-cmd may be set,
  ;; so we explicitly test it here.
  (when proof-shell-cd-cmd
    (proof-cd)
    (proof-shell-wait)))

;;
;; Commands which require an argument, and maybe affect the script.
;;

(proof-define-assistant-command-witharg proof-find-theorems
 "Search for items containing given constants."
 proof-find-theorems-command
 "Find theorems containing"
 (proof-shell-invisible-command arg))

(proof-define-assistant-command-witharg proof-issue-goal
 "Write a goal command in the script, prompting for the goal."
 proof-goal-command
 "Goal" ; Goals always start at a new line
 (let ((proof-next-command-on-new-line t))
   (proof-issue-new-command arg)))

(proof-define-assistant-command-witharg proof-issue-save
 "Write a save/qed command in the script, prompting for the theorem name."
 proof-save-command
 "Save as" ; Saves always start at a new line
 (let ((proof-next-command-on-new-line t))
   (proof-issue-new-command arg)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Electric terminator mode
;;

;; Register proof-electric-terminator as a minor mode.
(or (assq 'proof-electric-terminator-enable minor-mode-alist)
    (setq minor-mode-alist
	  (append minor-mode-alist
		  (list '(proof-electric-terminator-enable
			  (:eval
			   (if (eq major-mode proof-mode-for-script)
			       proof-terminal-string)))))))

;;;###autoload
(defun proof-electric-terminator-enable (&optional arg)
  "Ensure modeline update to display new value for electric terminator.
This a function is called by the custom-set property 'proof-set-value.
It can also be used as a minor mode function: with ARG, turn on iff ARG>0"
  (unless (null arg)
    (setq proof-electric-terminator-enable (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(proof-deftoggle proof-electric-terminator-enable
		 proof-electric-terminator-toggle)

(defun proof-electric-terminator (&optional count)
  "Insert terminator char, maybe sending the command to the assistant.
If we are inside a comment or string, insert the terminator.
Otherwise, if the variable `proof-electric-terminator-enable'
is non-nil, the command will be sent to the assistant.

To side-step the electric action and possibly insert multiple characters,
pass a non-nil COUNT arg, e.g. a numeric prefix such as M-3 <terminator>."
  (interactive "P")
  (if (and
       (not count)
       proof-electric-terminator-enable
       (not (proof-inside-comment (point)))
       (not (proof-inside-string (point))))
      (proof-assert-electric-terminator)
    (self-insert-command (prefix-numeric-value count))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion based on <PA>-completion-table
;;
;; Requires completion.el package.  Completion is usually a hand-wavy
;; thing, so we don't attempt to maintain a precise completion table.
;;

(defun proof-add-completions ()
  "Add completions from <PA>-completion-table to completion database.
Uses `add-completion' with a negative number of uses and ancient
last use time, to discourage saving these into the users database."
  (interactive)
  (require 'completion)
  (defvar completion-min-length)
  (declare-function add-completion "completion"
                    (string &optional num-uses last-use-time))
  (when (> (length proof-assistant) 0)
    (mapcar (lambda (cmpl)
	      ;; completion gives error; trapping is tricky so test again
	      (if (>= (length cmpl) completion-min-length)
		  (add-completion cmpl -1000 0)))
	    (proof-ass completion-table))))

;; NB: completion table is expected to be set when proof-script
;; is loaded!  Call `proof-script-add-completions' to update.

(eval-after-load "completion"
  '(proof-add-completions))

(defun proof-script-complete (&optional arg)
  "Like `complete' but case-fold-search set to `proof-case-fold-search'."
  (interactive "*p")
  ;; completion not autoloaded in GNU Emacs
  ;; FIXME: It's probably because we shouldn't use it ;-)
  (require 'completion)
  (declare-function complete "completion" (&optional arg))
  (let ((case-fold-search proof-case-fold-search))
    (complete arg)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Check validity of proofs
;;

(defun proof-check-annotate-source (proof-results annotate-passing)
  "Annotate proofs in current buffer according to PROOF-RESULTS.
PROOF-RESULTS must be the return value of `proof-check-proofs' or
`proof-check-chunks' for the current buffer. This function
annotates failing proofs with a comment containing \"FAIL\" and,
if annotate-passing is non-nil, also passing proofs with
\"PASS\". The annotation will be right aligned in the line
according to `proof-check-annotate-position'. The position for
right alignment is configured in
`proof-check-annotate-right-margin'. Existing \"PASS\" or
\"FAIL\" annotations are deleted together with the surrounding
white space."
  (let ((goal-column (or proof-check-annotate-right-margin fill-column)))
    (save-excursion
      (dolist (pr (nreverse proof-results))
        (let ((pass (car pr))
              (annot-pos
               (cond ((eq proof-check-annotate-position 'theorem) (nth 2 pr))
                     ((eq proof-check-annotate-position 'proof-using) (nth 3 pr))
                     (t (error
                         "Invalid value in `proof-check-annotate-position'"))))
              eol-pos fill)
          (goto-char annot-pos)
          ;; Now at the start of the span. Usually, this is right
          ;; behind the previous command, at the end of the line. Move
          ;; to the start of the command.
          (re-search-forward "[[:blank:]\n]*")
          (end-of-line)
          (when (looking-back "(\\* \\(PASS\\|FAIL\\) \\*)")
            (delete-char -10)
            (delete-horizontal-space))
          (when (or annotate-passing (not (car pr)))
            ;; Maybe there was no annotation in this line before, need
            ;; to delete trailing space.
            (delete-horizontal-space)
            (setq eol-pos (point))
            (beginning-of-line)
            (setq fill (- goal-column (- eol-pos (point)) 10))
            (when (< fill 1)
              (setq fill 1))
            (end-of-line)
            (insert (make-string fill ? )
                    "(* " (if pass "PASS" "FAIL") " *)")))))))
          
(defun proof-check-generate-report (proof-results tap batch)
  "Report `proof-check-proofs' results in PROOF-RESULTS in special buffer.
Report the results of `proof-check-proofs' in buffer
`proof-check-report-buffer' in human readable form or, if TAP is
not nil, in test anything protocol (TAP). If BATCH is not nil,
report the results via message, such that they appear on stdout
when Emacs runs in batch mode or, when BATCH is a string, write
the results to the file denoted by BATCH."
  (let* ((ok-fail (seq-group-by #'car proof-results))
         (frmt "  %-4s %s")
         (frmt-face (propertize frmt 'face 'error))
         (count 1)
         (inhibit-read-only t)          ; for the report buffer, below
         coq-proj-dir src-file)

    ;; determine a relative file name for current buffer
    (when buffer-file-name
      (setq coq-proj-dir (locate-dominating-file buffer-file-name
                                                 coq-project-filename)))
    (if coq-proj-dir
        (setq src-file (file-relative-name buffer-file-name coq-proj-dir))
      (setq src-file (buffer-name)))

    ;; generate header
    (with-current-buffer (get-buffer-create proof-check-report-buffer)
      (read-only-mode)
      (buffer-disable-undo)
      (erase-buffer)
      (if tap
          (insert (format "TAP version 13\n1..%d\n" (length proof-results)))
        ;; human output
        (insert
         (propertize (concat "Proof check results for " src-file) 'face 'bold)
         "\n\n")
        (insert
         (format
          (propertize "%d opaque proofs recognized: %d successful " 'face 'bold)
          (length proof-results)
          (length (cdr (assoc t ok-fail)))))
        (insert (format (propertize "%d FAILING" 'face 'error 'face 'bold)
                        (length (cdr (assoc nil ok-fail)))))
        (insert "\n\n"))
      (dolist (pr proof-results)
        (if tap
            (progn
              (insert (format "%sok %d %s\n"
                              (if (car pr) "" "not ")
                              count
                              (cadr pr)))
              (setq count (1+ count)))
          ;; human readable
          (insert (format (if (car pr) frmt frmt-face)
                          (if (car pr) "OK  " "FAIL")
                          (cadr pr)))
          (insert "\n")))
      (if batch
          (progn
            (insert "\n\n")
            (if (stringp batch)
                (write-region (point-min) (point-max) batch)
              (message "%s"
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun proof-check-chunks (chunks)
  "Worker function for `proof-check-proofs' for processing CHUNKS.
CHUNKS must be the reversed result of `proof-script-omit-filter'
for a whole buffer. (Only the top-level must be reversed, the
commands inside the chunks must be as returned by
`proof-script-omit-filter', that is in reversed order.) CHUNKS
must not contain any 'nested-proof chunk.

This function processes the content of CHUNKS normally by
asserting them one by one. Any error reported inside a 'no-proof
chunk is reported as error to the user. 'proof chunks containing
errors are silently replaced by
`proof-script-proof-admit-command'. The result is a list of proof
status results, one for each 'proof chunk in the same order. Each
proof-status result is a list of 4 elements as follows.
- 1st: proof status as `t' or `nil'. Proofs closed with a match
  of `proof-omit-cheating-regexp', if defined, count as failing,
  i.e., their status is `nil'.
- 2nd: the name of the proof as reported by
  `proof-get-proof-info-fn'.
- 3rd: Position of the start of the span containing the theorem
  to prove. More precisely, it is the second last span of the
  'no-proof chunk before the 'proof chunk. Note that spans
  usually contain preceeding white space. Therefore this position
  is not necessarily the first letter of the keyword introducing
  the theorem statement.
- 4rd: Position of the start of the span containing \"Proof
  using\". More precisely, it is the last span in the 'no-proof
  chunk before the proof."
  (let (proof-results current-proof-state-and-name proof-start-points)
    (while chunks
      (let* ((chunk (car chunks))       ; cdr at end
             (this-mode (car chunk))
             (next-mode (car (car (cdr chunks))))
             (vanillas-rev (nth 1 chunk))
             ;; add 'empty-action-list flag to last item to avoid the
             ;; call to `proof-shell-empty-action-list-command'
             (last-item (car vanillas-rev))
             (2nd-last-item (cadr vanillas-rev))
             (last-span-end (span-end (car last-item)))
             (new-last-item (list (nth 0 last-item) (nth 1 last-item)
                                  (nth 2 last-item)
                                  (cons 'empty-action-list (nth 3 last-item))))
             (vanillas-rev-updated (cons new-last-item (cdr vanillas-rev)))
             error
             cheated)
        ;; if this is a proof chunk the next must be no-proof or must not exist
        (cl-assert (or (not (eq this-mode 'proof))
                       (or (eq next-mode 'no-proof) (eq next-mode nil)))
                   nil "proof-check: two adjacent proof chunks")
        (proof-set-queue-endpoints (proof-unprocessed-begin) last-span-end)
        (proof-add-to-queue (nreverse vanillas-rev-updated) 'advancing)
        (proof-shell-wait)
        ;; (redisplay)
        (unless (eq last-span-end
                    (and proof-locked-span (span-end proof-locked-span)))
          ;; not all the spans have been asserted - there was some error
          (setq error t))
        (when (and error (eq this-mode 'no-proof))
          ;; all non-opaque stuff should be error free, abort and tell
          ;; the user
          (goto-char (proof-unprocessed-begin))
          (when (looking-at "$")
            (forward-line 1))
          (error "Error encountered outside opaque proofs after line %d"
                 (line-number-at-pos)))
            
        (cond
         ((and (eq this-mode 'no-proof) (eq next-mode 'proof))
          ;; non-opaque stuff has been processed error free, next
          ;; chunk is an opaque proof - record information needed next
          ;; round
          (setq current-proof-state-and-name
                (funcall proof-get-proof-info-fn))
          (cl-assert (cadr current-proof-state-and-name)
                     nil "proof-check: no proof name at proof start")
          (setq proof-start-points
                (list (span-start (car 2nd-last-item)) ; start of Lemma
                      (span-start (car last-item)))))  ; start of Proof using

         ((eq this-mode 'proof)         ; implies next-mode is no-proof
          ;; opaque proof chunk processed - with or without error
          (when error
            ;; opaque proof failed, retract, admit, and record error
            (proof-add-to-queue
             (list
              (list nil (list (funcall proof-retract-command-fn
                                       (car current-proof-state-and-name)))
                    'proof-done-invisible (list 'invisible))
              (list nil (list proof-script-proof-admit-command)
                    'proof-done-invisible (list 'invisible)))
             'advancing)
            (proof-shell-wait)
            (proof-set-locked-end last-span-end)
            (cl-assert (not (cadr (funcall proof-get-proof-info-fn)))
                       nil "proof-check: still in proof after admitting"))
          (when (and proof-omit-cheating-regexp
                     ;; check if somebody cheated - cheated proofs
                     ;; should count as failing
                     (proof-string-match
                      proof-omit-cheating-regexp
                      (mapconcat #'identity (nth 1 last-item) " ")))
            (setq cheated t))
          (push
           (cons (not (or error cheated))
                 (cons (cadr current-proof-state-and-name) proof-start-points))
           proof-results)))
        
        (setq chunks (cdr chunks))))
    (nreverse proof-results)))
    
(defun proof-check-proofs ()
  "Check proofs in current buffer and return a list of proof status results.
This is the internal worker for `proof-check-report' and
`proof-check-annotate'. 

Reset scripting and then check all opaque proof in the current
buffer, relying on the omit-proofs feature, see
`proof-script-omit-filter' and `proof-omit-proofs-configured'.
The results are returned as list of proof status results (a list
of 4-element lists), see `proof-check-chunks'.

This function does not (re-)compile required files."
  (unless (and proof-omit-proofs-configured
               proof-get-proof-info-fn
               proof-retract-command-fn)
    (error
     "proof-check-proofs has not been configured for your proof assistant"))
  ;; kill proof assistant and retract completely
  (when (buffer-live-p proof-shell-buffer)
    (proof-shell-exit t))
  ;; initialize scripting - taken from `proof-assert-until-point'
  (proof-activate-scripting nil 'advancing)
  (let* ((semis (proof-segment-up-to-using-cache (point-max)))
    	 (vanillas (proof-semis-to-vanillas
                    semis
                    '(no-response-display no-goals-display)))
         (chunks-rev (proof-script-omit-filter vanillas))
         (last-chunk (car chunks-rev))
         (chunks (nreverse chunks-rev))
         proof-results)
    (when (eq (car last-chunk) 'nested-proof)
      (error "Nested proof detected at line %d" (nth 2 last-chunk)))
    (cl-assert (not (eq (caar chunks) 'proof))
               nil "proof-check: first chunk cannot be a proof")
    (setq proof-results (proof-check-chunks chunks))
    (proof-shell-exit t)
    proof-results))

(defun proof-check-report (tap &optional batch)
  "Generate an overview about valid and invalid proofs.
This command completely processes the current buffer and
generates an overview about all the opaque proofs in it and
whether their proof scripts are valid or invalid. Note that
proofs closed with a cheating command (see
`proof-omit-cheating-regexp'), i.e., Admitted for Coq, count as
invalid.

This command makes sense for a development process where invalid
proofs are permitted and vos compilation and the omit proofs
feature (see `proof-omit-proofs-configured') are used to work at
the most interesting or challenging point instead of on the first
invalid proof.

Argument TAP, which can be set by a prefix argument, controls the
form of the generated overview. Nil, without prefix, gives an
human readable overview, otherwise it's test anything
protocol (TAP). Argument BATCH controls where the overview goes
to. If nil, or in an interactive call, the overview appears in
`proof-check-report-buffer'. If BATCH is a string, it should be a
filename to write the overview to. Otherwise the overview is
output via `message' such that it appears on stdout when this
command runs in batch mode.

In the same way as the omit-proofs feature, this command only
tolerates errors inside scripts of opaque proofs. Any other error
is reported to the user without generating an overview. The
overview only contains those names of theorems whose proof
scripts are classified as opaque by the omit-proofs feature. For
Coq for instance, among others, proof scripts terminated with
'Defined' are not opaque and do not appear in the generated
overview.

Note that this command does not (re-)compile required files.
Dependencies must be compiled before running this commands, for
instance by asserting all require commands beforehand."
  (interactive "P")
  (proof-check-generate-report (proof-check-proofs) tap batch))

(defun proof-check-annotate (annotate-passing &optional save-buffer)
  "Annotate failing proofs in current buffer with a \"FAIL\" comment.
This function modifies the current buffer in place. Use with
care!

Similarly to `proof-check-report', check all opaque proofs in the
current buffer. Instead of generating a report, failing proofs
are annotated with \"FAIL\" in a comment. Existing \"PASS\" or
\"FAIL\" comments (e.g., from a previous run) are deleted
together with the surrounding white space. With prefix argument
(or when ANNOTATE-PASSING is non-nil) also passing proofs are
annotated with a \"PASS\" comment. Pass and fail comments can be
placed at the last or second last statement before the opaque
proof. For Coq this corresponds to the proof using and the
theorem statement, respectively. In both cases the comment is
placed at the right margin of the first line, see
`proof-check-annotate-position' and
`proof-check-annotate-right-margin'.

Interactively, this command does not save the current buffer
after placing the annotations. With SAVE-BUFFER non-nil, the
current buffer is saved if it has been modified."
  (interactive "P")
  (proof-check-annotate-source (proof-check-proofs) annotate-passing)
  (when save-buffer
    (save-buffer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Span manipulation
;;

(defun pg-copy-span-contents (span)
  "Copy contents of SPAN to kill ring, sans surrounding whitespace."
  (copy-region-as-kill
   (save-excursion
     (goto-char (span-start span))
     (skip-chars-forward " \t\n")
     (point))
   (save-excursion
     (goto-char (span-end span))
     (skip-chars-backward " \t\n")
     (point)))
  (if (fboundp 'own-clipboard)		;; XEmacs function
      (own-clipboard (car kill-ring))))

(defun pg-numth-span-higher-or-lower (span num &optional noerr)
  "Find NUM'th span after/before SPAN.  NUM is positive for after."
  (unless (and span (<= (span-end span) (proof-unprocessed-begin)))
    (if noerr
	nil
      (error "No processed region under point")))
  (let ((downflag (> num 0))
	(num      (abs num))
	nextspan)
    (while (and (> num 0)
		(setq nextspan (if downflag
				   (next-span span 'type)
				 (prev-span span 'type)))
		(if downflag
		    ;; If moving down, check we don't go beyond
		    ;; end of processed region
		    (<= (span-end span) (proof-unprocessed-begin))
		  t))
      (setq num (1- num))
      (setq span nextspan))
    (if (= num 0)
	span
      (if noerr
	  nil
	(error "No region to move past")))))

(defun pg-control-span-of (span)
  "Return the controlling span of SPAN, or SPAN itself."
  (or (span-property span 'controlspan)
      span))

;; Really a drag-and-drop interface for this would be nice.
(defun pg-move-span-contents (span num)
  "Move SPAN up/downwards in the buffer, past NUM spans.
If NUM is negative, move upwards.  Return new span."
  (save-excursion
    (let  ((downflag (> num 0)) nextspan)
      ;; Always move a control span instead; it contains
      ;; children span which move together with it.
      (setq span (pg-control-span-of span))
      (setq nextspan (pg-numth-span-higher-or-lower span num))
      ;; We're going to move the span to before/after nextspan.
      ;; First make sure inserting there doesn't extend the span.
      (if downflag
	  (span-set-property nextspan 'end-open t)
	(span-set-property nextspan 'start-open t))
      ;; When we delete the span, we want to duplicate it
      ;; to recreate in the new position.
      (span-set-property span 'duplicable 't)
      ;; TODO: this is faulty: moving span up gives children
      ;; list with single nil element.  Hence liveness test
      (mapc (lambda (s) (if (span-live-p s)
			    (span-set-property s 'duplicable 't)))
	      (span-property span 'children))
      (let* ((start     (span-start span))
	     (end       (span-end span))
	     (contents  (buffer-substring start end))
	     ;; Locked end may move up when we delete
	     ;; region: we'll make sure to reset it
	     ;; again later, it shouldn't change.
	     ;; NB: (rely on singlethreadedness here, so
	     ;; lockedend doesn't move while in this code).
	     (lockedend (span-end proof-locked-span)))
	(let ((inhibit-read-only t))
	  ;; TODO: undo behaviour isn't quite right yet.
	  (undo-boundary)
	  (delete-region start end)
	  (let ((insertpos (if downflag
			       (span-end nextspan)
			     (span-start nextspan))))
	    (goto-char insertpos)
	    ;; Let XEmacs duplicate extents as needed, then repair
	    ;; their associations
	    (insert contents)
	    (let ((new-span
		   (span-at insertpos 'type)));should be one we deleted.
	      (span-set-property new-span 'children
				 (pg-fixup-children-spans
				  new-span insertpos (point)))
	      (span-set-end proof-locked-span lockedend)
	      (undo-boundary)
	      new-span)))))))

(defun pg-fixup-children-spans (new-parent start end)
  (append
   (span-mapcar-spans
    (lambda (span)
      (if (span-property span 'controlspan)
	  (progn
	    (span-set-property span 'controlspan new-parent)
	    (list span))))
    start end 'type)))

(defun pg-move-region-down (&optional num)
  "Move the region under point downwards in the buffer, past NUM spans."
  (interactive "p")
  (let ((span  (span-at (point) 'type)))
    (and span
	 (goto-char (span-start
		     (pg-move-span-contents span num)))
	 (skip-chars-forward " \t\n"))))

(defun pg-move-region-up (&optional num)
  "Move the region under point upwards in the buffer, past NUM spans."
  (interactive "p")
  (pg-move-region-down (- num)))

;; No key-binding is defined for these two functions:
(defun pg-show-all-proofs ()
  "Display all completed proofs in the buffer."
  (interactive)
  (pg-show-all-portions 'proof))

(defun pg-hide-all-proofs ()
  "Hide all completed proofs in the buffer."
  (interactive)
  (pg-show-all-portions 'proof 'hide))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Context menus inside spans
;;

(defun pg-pos-for-event (event)
  "Return position corresponding to position of a mouse click EVENT."
  (with-current-buffer
      (window-buffer (posn-window (event-start event)))
    (posn-point (event-start event))))

(defun pg-span-for-event (event)
  "Return span corresponding to position of a mouse click EVENT."
  (span-at (pg-pos-for-event event) 'type))

(defun pg-span-context-menu (event)
  "Display a context sensitive menu for proof script, around EVENT."
  (interactive "e")
  (let* ((span (pg-span-for-event event))
	 cspan)
    (when span
      ;; Find controlling span
      (while (setq cspan (span-property span 'controlspan))
	(setq span cspan))
      (let*
	  ((idiom (and span (span-property span 'idiom)))
	   (id    (and span (span-property span 'id))))
	(popup-menu (pg-create-in-span-context-menu
		     span idiom
		     (if id (symbol-name id))))))))

(defun pg-toggle-visibility ()
  "Toggle visibility of region under point."
  (interactive)
  (let* ((span (span-at (point) 'type))
	 (idiom (and span (span-property span 'idiom)))
	 (id    (and span (span-property span 'id))))
    (and  idiom id
	 (pg-toggle-element-visibility idiom (symbol-name id)))))

(defun pg-create-in-span-context-menu (span idiom name)
  "Create the dynamic context-sensitive menu for a span."
  (append
   (list (pg-span-name span))
   (list (vector
	  "Show/hide"
          (if idiom (list 'pg-toggle-element-visibility `(quote ,idiom) name))
	  (not (not idiom))))
   (list (vector
	  "Copy"       (list 'pg-copy-span-contents span) t))
   (list (vector
	  "Undo"
	  (list 'pg-span-undo span) t))
   ;; PG 4.1: these commands are neither very useful nor reliable
   ;; (list (vector
   ;; 	  "Move up"     (list 'pg-move-span-contents span -1)
   ;; 	  (pg-numth-span-higher-or-lower (pg-control-span-of span) -1 'noerr)))
   ;; (list (vector
   ;; 	  "Move down"   (list 'pg-move-span-contents span 1)
   ;; 	  (pg-numth-span-higher-or-lower (pg-control-span-of span) 1 'noerr)))
   (if proof-script-span-context-menu-extensions
       (funcall proof-script-span-context-menu-extensions span idiom name))
   (if proof-shell-theorem-dependency-list-regexp
       (proof-dependency-in-span-context-menu span))))

(defun pg-span-undo (span)
  "Undo to the start of the given SPAN."
  (interactive)
  (goto-char (span-start span))
  (proof-retract-until-point))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Message buffer hints
;;

(defun pg-goals-buffers-hint ()
  (pg-hint "Use \\<proof-mode-map>\\[proof-display-some-buffers] to rotate output buffers; \\<proof-mode-map>\\[pg-response-clear-displays] to clear response & trace."))

;;;###autoload
(defun pg-slow-fontify-tracing-hint ()
  (pg-hint "Large tracing output; refreshing intermittently.  Use \\<proof-mode-map>\\[pg-response-clear-displays] to clear trace."))

;;;###autoload
(defun pg-response-buffers-hint (&optional nextbuf)
  (unless (not (buffer-live-p proof-goals-buffer))
    (pg-hint
     (format
      "\\[proof-prf] for goals;%s \\[proof-layout-windows] refreshes"
      (if (or proof-three-window-enable
	      proof-multiple-frames-enable)
	  ""
	(format " \\[proof-display-some-buffers] rotates output%s;"
		(if nextbuf (concat " (next:" nextbuf ")") "")))))))

;;;###autoload
(defun pg-jump-to-end-hint ()
  (pg-hint "Use \\[proof-goto-end-of-locked] to jump to end of processed region"))

;;;###autoload
(defun pg-processing-complete-hint ()
  "Display hint for showing end of locked region or processing complete."
  (if (buffer-live-p proof-script-buffer)
      (let ((win (get-buffer-window proof-script-buffer)))
	(unless ;; end of locked already displayed
	    (and win (pos-visible-in-window-p (proof-unprocessed-begin)))
	  (with-current-buffer proof-script-buffer
	    (cond
	     ((proof-locked-region-empty-p)) ;; nothing if empty
	     ((proof-locked-region-full-p)
	      (pg-hint
	       (concat "Processing complete in " (buffer-name proof-script-buffer))))
	     ((not proof-autosend-running)
	      ;; partly complete: hint about displaying the locked end
	      (pg-jump-to-end-hint))))))))

;;;###autoload
(defun pg-next-error-hint ()
  "Display hint for locating error."
  (pg-hint "Use \\[proof-next-error] to go to next error location."))

;;;###autoload
(defun pg-hint (hintmsg)
  "Display a hint HINTMSG in the minibuffer, if `pg-show-hints' is non-nil.
The function `substitute-command-keys' is called on the argument."
  (if pg-show-hints (message (substitute-command-keys hintmsg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Symbol near point/identifier under mouse query
;;

(defun pg-identifier-under-mouse-query (event)
  "Query the prover about the identifier near mouse click EVENT."
  (interactive "e")
  (if proof-query-identifier-command
      (save-selected-window
	(save-excursion
	  (mouse-set-point event)
	  (pg-identifier-near-point-query)))))

;;;###autoload
(defun pg-identifier-near-point-query ()
  "Query the prover about the identifier near point.
If the result is successful, we add a span to the buffer which has
a popup with the information in it."
  (interactive)
  (let* ((stend       (if (region-active-p)
			  (cons (region-beginning) (region-end))
			(pg-current-word-pos)))
	 (start       (car-safe stend))
	 (end         (cdr-safe stend))
	 (identifier  (if start
			  (buffer-substring-no-properties
			   start end)))
	 (ctxt	      (if start
			  (save-excursion
			    (goto-char start)
			    (proof-buffer-syntactic-context)))))
    (if start
	(pg-identifier-query
	 identifier ctxt
	 ;; the callback
	 (lambda (_)
	   (save-excursion
	     (let ((idspan (span-make start end)))
               ;; (span-set-property idspan 'priority 90) ; highest
               (span-set-property idspan 'help-echo
                                  (pg-last-output-displayform)))))))))

(defvar proof-query-identifier-history nil
  "History for `proof-query-identifier'.")

(defun proof-query-identifier (string)
  "Query the prover about the identifier STRING.
If called interactively, STRING defaults to the current word near point."
  (interactive
   (list
    (completing-read "Query identifier: "
		     nil nil nil
		     (current-word)
		     'proof-query-identifier-history)))
  (if string (pg-identifier-query string)))

(defun pg-identifier-query (identifier &optional ctxt callback)
  "Query the proof assisstant about the given IDENTIFIER.
This uses `proof-query-identifier-command'.
Parameter CTXT allows to give a context for the identifier (which
allows for multiple name spaces).
If CALLBACK is set, we invoke that when the command completes."
  (unless (or (null identifier)
	      (string-equal identifier "")) ;; or whitespace?
    (proof-shell-invisible-command
     (cond
      ((stringp proof-query-identifier-command)
       ;; simple customization
       (format proof-query-identifier-command identifier))
      (t
       ;; buffer-syntactic context dependent, as an alist
       ;; (handy for Isabelle: not a true replacement for parsing)
       (format (nth 1 (assq ctxt proof-query-identifier-command))
	       identifier)))
     nil ; no wait
     callback)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imenu and Speedbar
;;

(declare-function speedbar-add-supported-extension "speedbar")
(eval-after-load "speedbar"
  '(and proof-assistant-symbol ;; *should* be set by now
	(speedbar-add-supported-extension
	 (nth 2 (assoc proof-assistant-symbol proof-assistant-table)))))

;;;###autoload
(defun proof-imenu-enable ()
  "Add or remove index menu."
  ;; NB: next two a bit interferring, but we suppose use-case is PG.
  (which-function-mode (if proof-imenu-enable 1 0))
  (when (listp which-func-modes)
    ;; FIXME: It's not PG's business to decide whether to use
    ;; which-function-mode.
    (add-to-list 'which-func-modes proof-mode-for-script))
  (if proof-imenu-enable
      (imenu-add-to-menubar "Index")
    (progn
      (when (listp which-func-modes)
        (setq which-func-modes
              (remove proof-mode-for-script which-func-modes)))
      (let ((oldkeymap (keymap-parent (current-local-map))))
	(if ;; sanity checks in case someone else set local keymap
	    (and oldkeymap
		 (lookup-key (current-local-map) [menu-bar index])
		 (not
		  (lookup-key oldkeymap [menu-bar index])))
	    (use-local-map oldkeymap)))
      (remove-hook 'menu-bar-update-hook 'imenu-update-menubar))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Command history
;;
;; This implements a history ring for commands in the locked region.
;; Inspired by (and code heavily copied from) comint.
;;
;; The current behaviour is not ideal: we only extend the input ring as
;; we process (so history does not browse pink text while the
;; prover is busy).  Moreover, instead of using a history, we might
;; simply parse commands backwards (or forwards) in the buffer.
;; (i.e, more like the copying behaviour implemented in Bibtex mode).
;;

(defvar pg-input-ring nil
  "Ring of previous inputs.")

(defvar pg-input-ring-index nil
  "Position of last matched command.")

(defvar pg-stored-incomplete-input nil
  "Stored incomplete input: string between point and locked.")

(defun pg-previous-input (arg)
  "Cycle backwards through input history, saving input.
If called interactively, ARG is given by the prefix argument."
  (interactive "*p")
  (if (and pg-input-ring-index
	   (or			       ; leaving the "end" of the ring
	    (and (< arg 0)	       ; going down
		 (eq pg-input-ring-index 0))
	    (and (> arg 0)		; going up
		 (eq pg-input-ring-index
		     (1- (ring-length pg-input-ring)))))
	   pg-stored-incomplete-input)
      (pg-restore-input)
    (pg-previous-matching-input "." arg)))

(defun pg-next-input (arg)
  "Cycle forwards through input history.
If called interactively, ARG is given by the prefix argument."
  (interactive "*p")
  (pg-previous-input (- arg)))

(defun pg-delete-input ()
  (let* ((unproc (proof-unprocessed-begin))
	 (start  (save-excursion
		   (goto-char unproc)
		   (skip-chars-forward " \t\n")
		   (point)))
	 (end    (point-at-eol)))
    (cond
     ((< start end)
      (delete-region start end))
     ((< start (point-at-eol))
      (delete-region start (point-at-eol))))))

(defun pg-get-old-input ()
  "Return text between end of locked region and point, up to EOL.
If there is no text, return the empty string."
  (let* ((unproc (proof-unprocessed-begin))
	 (start  (save-excursion
		   (goto-char unproc)
		   (skip-chars-forward " \t\n")
		   (point)))
	 (end    (point-at-eol)))
    (if (< start end)
	(buffer-substring-no-properties start end)
      nil)))


(defun pg-restore-input ()
  "Restore unfinished input."
  (interactive)
  (when pg-input-ring-index
    (pg-delete-input)
    (when (> (length pg-stored-incomplete-input) 0)
      (insert pg-stored-incomplete-input)
      (message "Input restored"))
    (setq pg-input-ring-index nil)))


(defun pg-search-start (arg)
  "Index to start a directional search, starting at `pg-input-ring-index'."
  (if pg-input-ring-index
      ;; If a search is running, offset by 1 in direction of arg
      (mod (+ pg-input-ring-index (if (> arg 0) 1 -1))
	   (ring-length pg-input-ring))
    ;; For a new search, start from beginning or end, as appropriate
    (if (>= arg 0)
	0				       ; First elt for forward search
      (1- (ring-length pg-input-ring)))))  ; Last elt for backward search


(defun pg-regexp-arg (prompt)
  "Return list of regexp and prefix arg using PROMPT."
  (let* (;; Don't clobber this.
	 (last-command last-command)
	 (regexp (read-from-minibuffer prompt nil nil nil
				       'minibuffer-history-search-history)))
    (list (if (string-equal regexp "")
	      (setcar minibuffer-history-search-history
		      (nth 1 minibuffer-history-search-history))
	    regexp)
	  (prefix-numeric-value current-prefix-arg))))

(defun pg-search-arg (arg)
  ;; First make sure there is a ring and that we are after the process mark
  (cond ((not (>= (point) (proof-unprocessed-begin)))
	 (error "Not in unlocked region"))
	((or (null pg-input-ring)
	     (ring-empty-p pg-input-ring))
	 (error "Empty input ring"))
	((zerop arg)
	 ;; arg of zero resets search from beginning, and uses arg of 1
	 (setq pg-input-ring-index nil)
	 1)
	(t
	 arg)))

(defun pg-previous-matching-input-string-position (regexp arg &optional start)
  "Return the index matching REGEXP ARG places along the input ring.
Moves relative to START, or `pg-input-ring-index'."
  (if (or (not (ring-p pg-input-ring))
	  (ring-empty-p pg-input-ring))
      (error "No history"))
  (let* ((len (ring-length pg-input-ring))
	 (motion (if (> arg 0) 1 -1))
	 (n (mod (- (or start (pg-search-start arg)) motion) len))
	 (tried-each-ring-item nil)
	 (prev nil))
    ;; Do the whole search as many times as the argument says.
    (while (and (/= arg 0) (not tried-each-ring-item))
      ;; Step once.
      (setq prev n
	    n (mod (+ n motion) len))
      ;; If we haven't reached a match, step some more.
      (while (and (< n len) (not tried-each-ring-item)
		  (not (string-match regexp (ring-ref pg-input-ring n))))
	(setq n (mod (+ n motion) len)
	      ;; If we have gone all the way around in this search.
	      tried-each-ring-item (= n prev)))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    ;; Now that we know which ring element to use, if we found it, return that.
    (if (string-match regexp (ring-ref pg-input-ring n))
	n)))

(defun pg-previous-matching-input (regexp n)
  "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match."
  (interactive (pg-regexp-arg "Previous input matching (regexp): "))
  (setq n (pg-search-arg n))
  (let ((pos (pg-previous-matching-input-string-position regexp n)))
    ;; Has a match been found?
    (if (null pos)
	(error "Match not found for regexp %s" regexp)
      ;; If leaving the edit line, save partial input
      (if (null pg-input-ring-index)	;not yet on ring
	  (setq pg-stored-incomplete-input (pg-get-old-input)))
      (setq pg-input-ring-index pos)
      (message "History item: %d" (1+ pos))
      (pg-delete-input)
      (insert (ring-ref pg-input-ring pos)))))

(defun pg-next-matching-input (regexp n)
  "Search forwards through input history for match for REGEXP.
\(Later history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, find the previous or Nth previous match."
  (interactive (pg-regexp-arg "Next input matching (regexp): "))
  (pg-previous-matching-input regexp (- n)))

(defvar pg-matching-input-from-input-string ""
  "Input previously used to match input history.")

;;;###autoload
(defun pg-previous-matching-input-from-input (n)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  (if (not (memq last-command '(pg-previous-matching-input-from-input
				pg-next-matching-input-from-input)))
      ;; Starting a new search
      (setq pg-matching-input-from-input-string (pg-get-old-input)
	    pg-input-ring-index nil))
  (if pg-matching-input-from-input-string
      (pg-previous-matching-input
       (concat "^" (regexp-quote pg-matching-input-from-input-string))
       n)
    (pg-previous-matching-input "." n)))

;;;###autoload
(defun pg-next-matching-input-from-input (n)
  "Search forwards through input history for match for current input.
\(Following history elements are more recent commands.)
With prefix argument N, search for Nth following match.
If N is negative, search backwards for the -Nth previous match."
  (interactive "p")
  (pg-previous-matching-input-from-input (- n)))



;;;###autoload
(defun pg-add-to-input-history (cmd)
   "Maybe add CMD to the input history.
CMD is only added to the input history if it is not a duplicate
of the last item added."
   (when (or (not (ring-p pg-input-ring))
	     (ring-empty-p pg-input-ring)
	     (not (string-equal (ring-ref pg-input-ring 0) cmd)))
     (unless (ring-p pg-input-ring)
       (setq pg-input-ring (make-ring pg-input-ring-size)))
     (ring-insert pg-input-ring cmd)))

;;;###autoload
(defun pg-remove-from-input-history (cmd)
  "Maybe remove CMD from the end of the input history.
This is called when the command is undone.  It's only
removed if it matches the last item in the ring."
  (if (and (ring-p pg-input-ring)
	   (not (ring-empty-p pg-input-ring))
	   (string-equal (ring-ref pg-input-ring 0) cmd))
      (ring-remove pg-input-ring 0)))


;;;###autoload
(defun pg-clear-input-ring ()
  (setq pg-input-ring nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Protected undo  (added in PG 4.0)
;;
;; From a suggestion of Stefan Monnier as an improvement over the
;; previous use of `undo-make-selective-list' to hack
;; `buffer-undo-list' in `proof-set-queue-endpoints'.
;;
;; Improved version due to Erik Martin-Dorel.  Uses auxiliary
;; functions `pg-protected-undo-1' and `next-undo-elt'
;;

(define-key proof-mode-map [remap undo] 'pg-protected-undo)
(define-key proof-mode-map [remap advertised-undo] 'pg-protected-undo)

(defun pg-protected-undo (&optional arg)
  "As `undo' but avoids breaking the locked region.

A numeric ARG serves as a repeat count.
If called interactively, ARG is given by the prefix argument.
If ARG is omitted, nil, or not numeric, it takes the value 1.

It performs each of the desired undos checking that these operations will
not affect the locked region, obeying `proof-strict-read-only' if required.
If strict read only behaviour is enforced, the user is queried whether to
retract before the undo is allowed.  If automatic retraction is enabled,
the retract and undo will go ahead without querying the user.

Moreover, undo/redo is always allowed in comments located in \
the locked region."
  (interactive "*P")
  (if (or (not proof-locked-span)
  	  (equal (proof-queue-or-locked-end) (point-min)))
      (undo arg)
    (let ((repeat ; Allow the user to perform successive undos at once
	   (if (numberp arg)
	       (prefix-numeric-value arg) ; arg is a raw prefix argument
	     1))
	  (newarg ; Allow the user to limit the undo to the current region
	   (and
	    ;; this Boolean expression is necessary to match
	    ;; the behavior of GNU Emacs (23.2) undo function
	    (or (region-active-p) (and arg (not (numberp arg))))
	    (> (region-end) (region-beginning)))))
      (while (> repeat 0)
	(pg-protected-undo-1 newarg) ; do some safe undos step by step
	(setq last-command 'undo) ; need for this assignment meanwhile
	(cl-decf repeat)))))

(defun pg-protected-undo-1 (arg)
  "This function is intended to be called by `pg-protected-undo'.

The flag ARG is passed to functions `undo' and `next-undo-elt'.
It should be a non-numeric value saying whether an undo-in-region
behavior is expected."
;; Note that if ARG is non-nil, (> (region-end) (region-beginning)) must hold,
;; at least for the first call from the loop of `pg-protected-undo'.
  (setq arg (and arg (not (numberp arg)))) ; ensure arg is boolean
  (if (or (not proof-locked-span)
	  (equal (proof-queue-or-locked-end) (point-min))) ; required test
      (undo arg)
    (let* ((next (next-undo-elt arg))
	   (delta (undo-delta next))  ; can be '(0 . 0) if next is nil
	   (beg (car delta))
	   (end (max beg (- beg (cdr delta))))) ; Key computation
      (when (and next (> beg 0)		; the "next undo elt" exists
		 (> (proof-queue-or-locked-end) beg)
		 proof-strict-read-only ; edit freely doesn't retract
		 (not (and		; neither does edit in comments
		       (proof-inside-comment beg)
		       (proof-inside-comment end))))
	(if (or (eq proof-strict-read-only 'retract)
		(y-or-n-p "Next undo will modify read-only region, retract? "))
	    (proof-retract-before-change beg end)
	  (when (eq last-command 'undo) (setq this-command 'undo))
	  ;; now we can stop the function without breaking possible undo chains
	  (error
	   "Cannot undo without retracting to the appropriate position")))
      (undo arg))))

(defun next-undo-elt (arg)
  "Return the undo element that will be processed on next undo/redo.
Assume the undo-in-region behavior will apply if ARG is non-nil."
  (let ((undo-list (if arg		; handle "undo in region"
		       (undo-make-selective-list
			(region-beginning) (region-end)) ; can be '(nil)
		     buffer-undo-list)))		 ; can be nil
    (if (or (null undo-list) (equal undo-list (list nil)))
	nil				; there is clearly no undo elt
      (while (and undo-list             ; to ensure it will terminate
                  (let ((elt (car undo-list)))
                    (not (and (consp elt)
                              (or (stringp (car elt))
                                  (integerp (car elt)))))))
	(setq undo-list (cdr undo-list))) ; get the last undo record
      (if (and (eq last-command 'undo)
	       (or (eq pending-undo-list t)
		   (gethash undo-list undo-equiv-table)))
	  ;; then we are within a run of consecutive undo commands
	  (if (eq pending-undo-list t) nil (car pending-undo-list))
	(car undo-list)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automatic processing of buffer ahead of point
;;
;; Added in PG 4.0
;;

(defvar proof-autosend-timer nil "Timer used by autosend.")

(deflocal proof-autosend-modified-tick nil
  "Records 'buffer-chars-modified-tick' since last autosend.")

;;;###autoload
(defun proof-autosend-enable (&optional nomsg)
  "Enable or disable autosend behaviour."
  (if proof-autosend-timer
      (cancel-timer proof-autosend-timer))
  (when proof-autosend-enable
    (setq proof-autosend-timer
	  (run-with-idle-timer proof-autosend-delay
			       t 'proof-autosend-loop))
    (setq proof-autosend-modified-tick nil)
    (unless nomsg (message "Automatic sending turned on.")))
  (when (not proof-autosend-enable)
    (setq proof-autosend-timer nil)
    (unless nomsg (message "Automatic sending turned off."))))

(defun proof-autosend-delay ()
  "Adjust autosend timer when variable `proof-autosend-delay' changes."
  (proof-autosend-enable t))

(defun proof-autosend-loop ()
  (proof-with-current-buffer-if-exists proof-script-buffer
    (unless (or (proof-locked-region-full-p)
		proof-shell-busy
		;; TODO: re-engage autosend after C-c C-n even if not modified.
		(eq (buffer-chars-modified-tick) proof-autosend-modified-tick)
		(and proof-autosend-all
		     (eq proof-shell-last-queuemode 'retracting)))
      (let ((proof-autosend-running t))
	(setq proof-autosend-modified-tick (buffer-chars-modified-tick))
	(if proof-autosend-all
	    (proof-autosend-loop-all)
	  (proof-autosend-loop-next))))))

(defun proof-autosend-loop-all ()
  "Send commands from the script until an error, complete, or input appears."
  (message "Sending commands to prover...")
  (unwind-protect
      (progn
	(save-excursion
	  (goto-char (point-max))
	  (proof-assert-until-point
	   (if proof-multiple-frames-enable
	       'no-minibuffer-messages ; nb: not API
	     '(no-response-display
	       no-error-display
	       no-goals-display))))
	(proof-shell-wait t) ; interruptible
	(cond
	 ((eq proof-shell-last-output-kind 'error)
	  (message "Sending commands to prover...error"))
	 ((and (input-pending-p) proof-shell-busy)
	  (proof-interrupt-process)
	  (message "Sending commands to prover...interrupted")
	  (proof-shell-wait))
	 (t
	  (message "Sending commands to prover...done"))))))

(defun proof-autosend-loop-next ()
  "Send the next command from the script and indicate its success/otherwise."
  (unwind-protect
      (let ((qol   (proof-queue-or-locked-end)))
	(save-excursion
	  ;(goto-char qol)
	  ;(skip-chars-forward " \t\n")
	  (message "Trying next commands in prover...")
	  (proof-assert-until-point
	   (if proof-multiple-frames-enable
	       'no-minibuffer-messages ; nb: not API
	     '(no-response-display
	       no-error-display
	       no-goals-display))))
	(let ((proof-sticky-errors t))
	  (proof-shell-wait t)) ; interruptible
	(cond
	 ((eq proof-shell-last-output-kind 'error)
	  (message "Trying next commands in prover...error"))
	 ((and (input-pending-p) proof-shell-busy)
	  (proof-interrupt-process)
	  (message "Trying next commands in prover...interrupted")
	  (proof-shell-wait))
	 (t
	  (message "Trying next commands in prover...OK")))
	;; success: now undo in prover, highlight undone spans if OK
	(unless (eq qol (proof-queue-or-locked-end)) ; no progress
	  (save-excursion
	    (goto-char qol)
	    (proof-retract-until-point
	     (lambda (beg end)
	       (span-make-self-removing-span
		(save-excursion
		  (goto-char beg)
		  (skip-chars-forward " \t\n")
		  (point))
		end
		'face 'highlight))
	     '(no-response-display
	       no-error-display
	       no-goals-display)))))))
  
(provide 'pg-user)

;;; pg-user.el ends here
