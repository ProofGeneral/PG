;;; -*- lexical-binding: t -*-

;; coq.el --- Major mode for Coq proof assistant  -*- coding: utf-8 -*-
;; Copyright (C) 1994-2009 LFCS Edinburgh.
;; Authors: Healfdene Goguen, Pierre Courtieu
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)
;; Maintainer: Pierre Courtieu <Pierre.Courtieu@cnam.fr>

(eval-when-compile
  (require 'cl-lib)
  (require 'proof-compat))

(cl-eval-when (compile)
              (require 'proof-utils)
              (require 'span)
              (require 'outline)
              (require 'newcomment)
              (require 'etags)
              (require 'coq-queries)
              (unless (proof-try-require 'smie)
                (defvar smie-indent-basic nil)
                (defvar smie-rules-function nil))
              (defvar proof-info nil)       ; dynamic scope in proof-tree-urgent-action
              (defvar action nil)       ; dynamic scope in coq-insert-as stuff
              (defvar string nil)       ; dynamic scope in coq-insert-as stuff
              (defvar coq-auto-insert-as nil)    ; defpacustom
              (defvar coq-time-commands nil)        ; defpacustom
              (defvar coq-use-project-file t)        ; defpacustom
              (defvar coq-use-editing-holes nil)    ; defpacustom
              (defvar coq-hide-additional-subgoals nil) ; defpacustom
              (proof-ready-for-assistant 'coq))     ; compile for coq

(require 'cl-lib)
(require 'proof)
(require 'proof-utils)
(require 'proof-resolver)
(require 'coq-system)                   ; load path, option, project file etc.
(require 'coq-syntax)                   ; font-lock, syntax categories (tactics, commands etc)
(require 'coq-state-vars)               ; global state of the proof
(require 'coq-local-vars)               ; setting coq args via file variables 
                                        ;   (prefer _CoqProject file instead)
(require 'coq-abbrev)                   ; abbrev and coq specific menu
(require 'coq-seq-compile)              ; sequential compilation of Requires
(require 'coq-par-compile)              ; parallel compilation of Requires
(require 'coq-server)
(require 'coq-response)
(require 'coq-parsing)
(require 'coq-queries)
(require 'coq-header-line)
(require 'coq-xml)

;; for compilation in Emacs < 23.3 (NB: declare function only works at top level)
(declare-function smie-bnf->prec2 "smie")
(declare-function smie-rule-parent-p "smie")
(declare-function smie-default-forward-token "smie")
(declare-function smie-default-backward-token "smie")
(declare-function smie-rule-prev-p "smie")
(declare-function smie-rule-separator "smie")
(declare-function smie-rule-parent "smie")

(declare-function some "cl-extra")      ; spurious bytecomp warning

;; prettify is in emacs > 24.4
;; FIXME: this should probably be done like for smie above.
(defvar coq-may-use-prettify nil) ; may become t below
(eval-when-compile
  (if (fboundp 'prettify-symbols-mode)
      (defvar coq-may-use-prettify t)
    (defvar prettify-symbols-alist nil)))

;; ----- coq-server configuration options


;; ----- coq-shell configuration options

;;; Code:
;; debugging functions
;; (defun proofstack () (coq-get-span-proofstack (span-at (point) 'type)))
(defvar coq-debug nil)
;; End debugging

(defcustom coq-server-log-traffic t
  "In server mode, log traffic between emacs and coqtop to a buffer"
  :type 'boolean
  :group 'coq)

(defcustom coq-user-init-cmds nil
  "user defined init commands for Coq.
These are appended at the end of `coq-server-init-cmds'."
  :type '(repeat (cons (string :tag "command")))
  :group 'coq)

(defcustom coq-optimise-resp-windows-enable t
  "If non-nil (default) resize vertically response window after each command."
  :type 'boolean
  :group 'coq)

;; Default coq is only Private_ and _subproof
(defcustom coq-search-blacklist-strings ; add these? _ind _rect _rec
  (list "Private_" "_subproof")
  "Strings to blacklist for search requests to Coq environment."
  :type '(list string)
  :group 'coq)

;; add quotes to blacklist strings, concat into single string
(defun coq-format-blacklist-strings (bstrs)
  (mapconcat (lambda (s) (format "\"%s\"" s)) bstrs " "))

(defvar coq-formatted-search-blacklist-strings (coq-format-blacklist-strings coq-search-blacklist-strings))

;; this remembers the previous value of coq-search-blacklist-string, so that we
;; can cook a remove+add blacklist command each time the variable is changed.
;; initially we put it at current value of coq-search-blacklist-string.
(defvar coq-search-blacklist-strings-prev coq-search-blacklist-strings)

(defun coq--set-search-blacklist (s)
  (let ((cmd (format "Remove Search Blacklist %s. \nAdd Search Blacklist %s."
          (coq-format-blacklist-strings coq-search-blacklist-strings-prev) s)))
    (setq coq-search-blacklist-strings-prev coq-search-blacklist-strings)
    (lambda ()
      (list (coq-xml-add-item cmd) nil))))

(defpacustom search-blacklist coq-formatted-search-blacklist-strings
  "Strings to blacklist in requests to Coq environment."
  :type 'string
  :get coq-formatted-search-blacklist-strings
  :setting coq--set-search-blacklist)

(defcustom coq-prefer-top-of-conclusion nil
  "prefer start of the conclusion over its end when displaying goals
that do not fit in the goals window."
  :type 'boolean
  :group 'coq)

(defconst coq-server-init-cmds
  (list (coq-xml-init))
 "Commands to initialize Coq.")

(require 'coq-syntax)
;; FIXME: Even if we don't use coq-indent for indentation, we still need it for
;; coq-script-parse-cmdend-forward/backward and coq-find-real-start.
(require 'coq-indent)


;; FIXME da: this was disabled (set to nil) -- why?
;; da: 3.5: add experimental
;; am:answer: because of bad interaction
;; with coq -R option.
(defvar coq-shell-cd nil
  ;;  "Add LoadPath \"%s\"." ;; fixes unadorned Require (if .vo exists).
  "*Command of the inferior process to change the directory.")

(defvar coq-goal-regexp
  "\\(============================\\)\\|\\(subgoal [0-9]+\\)\n")

(defcustom coq-end-goals-regexp-show-subgoals "\n(dependent evars:"
  "Regexp for `proof-shell-end-goals-regexp' when showing all subgoals.
A setting of nil means show all output from Coq. See also
`coq-hide-additional-subgoals'."
  :type '(choice regexp (const nil))
  :group 'coq)

(defcustom coq-end-goals-regexp-hide-subgoals
  (concat "\\(\nsubgoal 2 \\)\\|\\(" coq-end-goals-regexp-show-subgoals "\\)")
  "Regexp for `proof-shell-end-goals-regexp' when hiding additional subgoals.
See also `coq-hide-additional-subgoals'."
  :type '(choice regexp (const nil))
  :group 'coq)

;;
;; prooftree customization
;;

(defgroup coq-proof-tree ()
  "Coq specific customization for prooftree."
  :group 'coq-config
  :package-version '(ProofGeneral . "4.2"))

;; Ignore all commands that start a proof. Otherwise "Proof" will appear
;; as superfluous node in the proof tree. Note that we cannot ignore Proof,
;; because, Fixpoint does not display the proof goal, see Coq bug #2776. 
(defcustom coq-proof-tree-ignored-commands-regexp
  (concat "^\\(\\(Show\\)\\|\\(Locate\\)\\|"
          "\\(Theorem\\)\\|\\(Lemma\\)\\|\\(Remark\\)\\|\\(Fact\\)\\|"
          "\\(Corollary\\)\\|\\(Proposition\\)\\|\\(Definition\\)\\|"
          "\\(Let\\)\\|\\(Fixpoint\\)\\|\\(CoFixpoint\\)\\)")
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

(defcustom coq-proof-tree-new-layer-command-regexp
  "^\\(\\(Proof\\)\\|\\(Grab Existential Variables\\)\\)"
  "Regexp for `proof-tree-new-layer-command-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-current-goal-regexp
  (concat "^[0-9]+ \\(?:focused \\)?subgoal\\(?:s\\)?\\s-*"
          "\\(?:(\\(?:unfocused: [-0-9]+\\)?,?"
          "\\s-*\\(?:shelved: [-0-9]+\\)?)\\)?\\(?:\\s-*, subgoal 1\\)? "
          "(ID \\([0-9]+\\))\n\\s-*\n\\(\\(?: .*\n\\)+\\)\\(?:\n\\|$\\)")
  "Regexp for `proof-tree-current-goal-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-update-goal-regexp
  (concat "^goal / evar \\([0-9]+\\) is:\n"
          "\\s-*\n\\(\\(?:.+\n\\)*\\)\\(?:\n\\|$\\)")
  "Regexp for `proof-tree-update-goal-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-additional-subgoal-ID-regexp
  "^subgoal [0-9]+ (ID \\([0-9]+\\)) is:"
  "Regexp for `proof-tree-additional-subgoal-ID-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-existential-regexp "\\(\\?[0-9]+\\)"
  "Regexp for `proof-tree-existential-regexp'."
  :type 'regexp
  :group 'coq-proof-tree)

(defcustom coq-proof-tree-instantiated-existential-regexp
  (concat coq-proof-tree-existential-regexp " using")
  "Regexp for recognizing an instantiated existential variable."
  :type 'regexp
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
          "\\(?:\\(?:<infomsg>\\)?No more subgoals\\)\\|"
          "\\(No more subgoals but non-instantiated "
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

;; FIXME, deal with interacive "Definition"
(defvar coq-outline-regexp
  ;;  (concat "(\\*\\|"
  (concat "[ ]*" (regexp-opt
                  '(
                    "Ltac" "Corr" "Modu" "Sect" "Chap" "Goal"
                    "Definition" "Lemm" "Theo" "Fact" "Rema"
                    "Mutu" "Fixp" "Func") t)))
;;)

(defvar coq-outline-heading-end-regexp "\\.[ \t\n]")

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

(eval-and-compile ;; FIXME: Why?
  (define-derived-mode coq-response-mode proof-response-mode
    "Coq Response" nil
    (coq-response-config)))

(eval-and-compile ;; FIXME: Why?
  (define-derived-mode coq-mode proof-mode "Coq"
    "Major mode for Coq scripts.

\\{coq-mode-map}"
    (coq-mode-config)))

(eval-and-compile ;; FIXME: Why?
  (define-derived-mode coq-goals-mode proof-goals-mode
    "Coq Goals" nil
    (coq-goals-mode-config)))

;; Indentation and navigation support via SMIE.

(defcustom coq-use-smie t
  "OBSOLETE. smie code is always used now.

If non-nil, Coq mode will try to use SMIE for indentation.
SMIE is a navigation and indentation framework available in Emacs >= 23.3."
  :type 'boolean
  :group 'coq)

(require 'smie nil 'noerror)
(require 'coq-smie nil 'noerror)

(defun coq-reset-all-state ()
  (coq-reset-state-vars)
  (coq-reset-tables)
  (coq-server--clear-response-buffer)
  (coq-server--clear-goals-buffer)
  (when proof-script-buffer
    (with-current-buffer proof-script-buffer
      (mapc 'span-delete (overlays-in (point-min) (point-max)))))
  (proof-init-segmentation))

;;
;; Auxiliary code for Coq modes
;;


(defun coq-remove-trailing-blanks (s)
  (let ((pos (string-match "\\s-*\\'" s)))
    (substring s 0 pos)))

(defun coq-remove-starting-blanks (s)
  (string-match "\\`\\s-*" s)
  (substring s (match-end 0) (length s)))

;;;;;;;;;;; Trying to accept { and } as terminator for empty commands. Actually
;;;;;;;;;;; I am experimenting two new commands "{" and "}" (without no
;;;;;;;;;;; trailing ".") which behave like BeginSubProof and EndSubproof. The
;;;;;;;;;;; absence of a trailing "." makes it difficult to distinguish between
;;;;;;;;;;; "{" of normal coq code (implicits, records) and this the new
;;;;;;;;;;; commands. We therefore define a coq-script-parse-function to this
;;;;;;;;;;; purpose.

;; coq-end-command-regexp is ni coq-indent.el
(defconst coq-script-command-end-regexp coq-end-command-regexp)
;;        "\\(?:[^.]\\|\\(?:\\.\\.\\)\\)\\.\\(\\s-\\|\\'\\)")



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

;; 
;; type status = {
;;   status_path : string list;
;;   (** Module path of the current proof *)
;;   status_proofname : string option;
;;   (** Current proof name. [None] if no focussed proof is in progress *)
;;   status_allproofs : string list;
;;   (** List of all pending proofs. Order is not significant *)
;;   status_proofnum : int;
;;   (** An id describing the state of the current proof. *)
;; }

(defun coq-in-proof ()
  (not (null coq-pending-proofs)))

;; Each time the state changes (hook below), (try to) put the state number in
;; the last locked span (will fail if there is already a number which should
;; happen when going back in the script).  The state number we put is not the
;; last one because the last one has been sent by Coq *after* the change. We
;; use `coq-last-but-one-state-id' instead and then update it.

;; hook for resizing windows
(add-hook 'proof-server-init-hook 'coq-optimise-resp-windows-if-option)

;; hook to count how many Adds we're about to send
(add-hook 'proof-server-enqueue-hook 'coq-server-count-pending-adds)

(defun count-not-intersection (l notin)
  "Return the number of elts of L that are not in NOTIN."
  (let ((l1 l) (l2 notin) (res 0))
    (while l1
      (if (member (car l1) l2) ()
        (setq res (+ res 1))) ; else
      (setq l1 (cdr l1)))
    res
    ))

(defun coq--find-previous-state-id (span)
  "Find state id for nearest span with a state id before SPAN."
  (with-current-buffer proof-script-buffer
    (let* ((all-spans (overlays-in (point-min) (1- (span-start span))))
           (state-id-spans (cl-remove-if-not 
                            (lambda (sp) (span-property sp 'state-id))
                            all-spans))
           ;; reverse sort, so that head of list is nearest SPAN
           (sorted-state-id-spans 
            (sort state-id-spans 
                  (lambda (sp1 sp2) (> (span-start sp1) (span-start sp2))))))
      (and (consp sorted-state-id-spans)
           (span-property (car sorted-state-id-spans) 'state-id)))))

;; send a command to coqtop via XML to do retraction
(defun coq-server-find-and-forget (span)
  "Backtrack to SPAN, possibly resulting in a full retraction. Send Edit_at for the 
nearest preceding span with a state id."
  (unless 
      ;; processed externally (i.e. Require, etc), nothing to do
      ;; (should really be unlocked when we undo the Require).
      (eq (span-property span 'type) 'proverproc) ; TODO is this needed?
    ;; remove any processing spans beneath this span
    ;; because we'll need to re-do then
    ;; we don't know the keys for these spans in the span table
    ;; that's OK, values are weakly held in that table
    (with-current-buffer proof-script-buffer
      (let* ((spans (overlays-in (span-end span) (point-max)))
             (processing-spans 
              (cl-remove-if-not 
               (lambda (sp) (span-property sp 'processing-in)) 
               spans)))
        (mapc 'span-delete processing-spans)))
    ;; if auto-retracting on error, leave error in response buffer
    (if (or coq-server-retraction-on-error
            coq-server-retraction-on-interrupt)
        (setq coq-server-retraction-on-error nil
              coq-server-retraction-on-interrupt nil)
      (coq-server--clear-response-buffer))
    ;; use nearest state id before this span; if none, retract buffer
    (if (and (= (span-start span) 1) coq-retract-buffer-state-id)
        (coq-server--send-retraction coq-retract-buffer-state-id t))
      (let ((prev-state-id (coq--find-previous-state-id span)))
        (if prev-state-id
            (coq-server--send-retraction prev-state-id t)
          (proof-retract-buffer)))))

(defvar coq-current-goal 1
  "Last goal that Emacs looked at.")

(defun coq-goal-hyp ()
  (cond
   ((looking-at "============================\n")
    (goto-char (match-end 0))
    (cons 'goal (int-to-string coq-current-goal)))
   ((looking-at "subgoal \\([0-9]+\\) is:\n")
    (goto-char (match-end 0))
    (cons 'goal (match-string 1))       ;FIXME: This is dead-code!?  --Stef
    (setq coq-current-goal (string-to-number (match-string 1))))
   ((proof-looking-at proof-shell-assumption-regexp)
    (cons 'hyp (match-string 1)))
   (t nil)))

(defun coq-state-preserving-p (cmd)
  ;; (or
  (proof-string-match coq-non-retractable-instruct-regexp cmd))
;; (and
;;  (not (proof-string-match coq-retractable-instruct-regexp cmd))
;;  (or
;;   (message "Unknown command, hopes this won't desynchronize ProofGeneral")
;;   t))))


(defun coq-hide-additional-subgoals-switch ()
  "Function invoked when the user switches `coq-hide-additional-subgoals'."
  (if coq-time-commands
      (progn
        (setq coq-hide-additional-subgoals nil)
        (error
         "You must disable ``Time Commands'' (var coq-time-commands) first"))
    (if coq-hide-additional-subgoals
        (setq proof-shell-end-goals-regexp coq-end-goals-regexp-hide-subgoals)
      (setq proof-shell-end-goals-regexp coq-end-goals-regexp-show-subgoals))))

(defun coq-time-commands-switch ()
  "Function invoked when the user switches `coq-time-commands'.
Resets `coq-hide-additional-subgoals' and puts nil into
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
  "Enumerates the different kinds of notation information one can get from Coq.")

;; helper for insert intros
(defun coq--format-intros (output)
  "Create an “intros” form from the OUTPUT of “Show Intros”."
  (let* ((shints (replace-regexp-in-string "[\r\n ]*\\'" "" output)))
    (if (or (string= "" shints)
	    (string-match coq-error-regexp shints))
	(error "Don't know what to intro")
      (format "intros %s" shints))))

(defun coq-insert-intros ()
  "Insert an intros command with names given by Show Intros.
Based on idea mentioned in Coq reference manual."
  (interactive)
  (proof-server-invisible-cmd-handle-result
   (coq-queries-show-intros-thunk)
   (lambda (response call span)
     (let ((intros (coq-queries-get-message-string response)))
       (if intros
         (with-current-buffer proof-script-buffer
           (indent-region (point)
                          (progn (insert (coq--format-intros intros))
                                 (save-excursion
                                   (insert " ")
                                   (point))))
           ;; `proof-electric-terminator' moves the point in all sorts of strange
           ;; ways, so we run it last
           (let ((last-command-event ?.)) ;; Insert a dot
             (proof-electric-terminator)))
         ;; if no intros, call default response handler
         (coq-server-process-response response call span))))))

(defun coq-check-document ()
  "Force coqtop to check validity of entire document."
  (interactive)
  (proof-server-send-to-prover (coq-xml-status 'true)))

(defun coq-find-theorems ()
  (coq-queries-ask "Find theorems" "Search" nil))

(defun coq-get-context ()
  (proof-server-invisible-command
   (coq-queries-print-all-thunk)))

(defun coq-PrintScope ()
  "Show information on Coq notations."
  (interactive)
  (let*
      ((mods
        (completing-read "Infos on notation (TAB to see list): "
                         notation-print-kinds-table))
       (s (read-string  "Name (empty for all): ")))
    (cond
     ((and (string-equal mods "Print Scope(s)") (string-equal s ""))
      (proof-invisible-command (coq-queries-print-scopes-thunk)))
     (t
      (proof-invisible-command (coq-queries-print-visibility-thunk))))))

(defcustom coq-remap-mouse-1 nil
  "Whether coq mode should remap mouse button 1 to coq queries.

This overrides the default global binding of (control mouse-1) and
(shift mouse-1) (buffers and faces menus). Hence it is nil by
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
    (save-selected-frame
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
         (proof-invisible-command
          (format (concat  cmd " %s . ")
                  ;; Notation need to be surrounded by ""
                  (if id id (concat "\"" notat "\"")))))))))

(defsubst coq-put-into-brackets (s)
  (concat "[ " s " ]"))

(defsubst coq-put-into-double-quote-if-notation (s)
  (if (coq-is-symbol-or-punct (string-to-char s))
      (concat "\"" s "\"")
    s))

(defun coq-build-removed-pattern (s)
  (concat " -\"" s "\""))

(defun coq-build-removed-patterns (l)
  (mapcar 'coq-build-removed-pattern l))

(defsubst coq-put-into-quotes (s)
  (concat "\"" s "\""))

(defun coq-SearchIsos ()
  "Search a term whose type is isomorphic to given type.
This is specific to `coq-mode'."
  (interactive)
  (coq-queries-ask
   "SearchPattern (parenthesis mandatory), ex: (?X1 + _ = _ + ?X1)"
   "SearchPattern" nil))

(defun coq-SearchConstant ()
  (interactive)
  (coq-queries-ask "Search constant" "Search"))

(defun coq-SearchRewrite ()
  (interactive)
  (coq-queries-ask "SearchRewrite" "SearchRewrite" nil))

;; N.B. SearchAbout was replaced bySearch in v8.5
;; so it's removed here

(defun coq-Print (with-printing-all)
  "Ask for an ident and print the corresponding term.
With flag Printing All if some prefix arg is given (C-u)."
  (interactive "P")
  (if with-printing-all
      (coq-queries-ask-show-all "Print" "Print")
    (coq-queries-ask "Print" "Print")))

(defun coq-Print-with-implicits ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-queries-ask-show-implicits "Print" "Print"))

(defun coq-Print-with-all ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-queries-ask-show-all "Print" "Print"))

(defun coq-About (withprintingall)
  "Ask for an ident and print information on it."
  (interactive "P")
  (if withprintingall
      (coq-queries-ask-show-all "About" "About")
    (coq-queries-ask "About" "About")))

(defun coq-About-with-implicits ()
  "Ask for an ident and print information on it."
  (interactive)
  (coq-queries-ask-show-implicits "About" "About"))

(defun coq-About-with-all ()
  "Ask for an ident and print information on it."
  (interactive)
  (coq-queries-ask-show-all "About" "About"))


(defun coq-LocateConstant ()
  "Locate a constant."
  (interactive)
  (coq-queries-ask "Locate" "Locate"))

(defun coq-LocateLibrary ()
  "Locate a library."
  (interactive)
  (coq-queries-ask "Locate Library" "Locate Library"))

(defun coq-LocateNotation ()
  "Locate a notation.  Put it automatically into quotes.
This is specific to `coq-mode'."
  (interactive)
  (coq-queries-ask
   "Locate notation (ex: \'exists\' _ , _)" "Locate"
   ))

(defun coq-Inspect ()
  (interactive)
  (coq-queries-ask "Inspect how many objects back?" "Inspect" t))

(defun coq-PrintSection()
  (interactive)
  (coq-queries-ask "Print Section" "Print Section" t))

(defun coq-Print-implicit ()
  "Ask for an ident and print the corresponding term."
  (interactive)
  (coq-queries-ask "Print Implicit" "Print Implicit"))

(defun coq-Check (withprintingall)
  "Ask for a term and print its type.
With flag Printing All if some prefix arg is given (C-u)."
  (interactive "P")
  (if withprintingall
      (coq-queries-ask-show-all "Check" "Check")
    (coq-queries-ask "Check" "Check")))

(defun coq-Check-show-implicits ()
  "Ask for a term and print its type."
  (interactive)
  (coq-queries-ask-show-implicits "Check" "Check"))

(defun coq-Check-show-all ()
  "Ask for a term and print its type."
  (interactive)
  (coq-queries-ask-show-all "Check" "Check"))

(defun coq-get-response-string-at (&optional pt)
  "Go forward from PT until reaching a 'response property, and return it.
Response span only starts at first non space character of a
command, so we may have to go forward to find it. Starts
from (point) if pt is nil. Precondition: pt (or point if nil)
must be in locked region."
  (let ((pt (or pt (point))))
    (save-excursion
      (goto-char pt)
      (while (and (not (eq (point) (point-max)))
                  (not (span-at (point) 'response)))
        (forward-char))
      (span-property (span-at (point) 'response) 'response))))

(defun coq-Show (with-printing-all)
  "Ask for a number i and show the ith goal.
Ask for a number i and show the ith current goal. With non-nil
prefix argument and not on the locked span, show the goal with
flag Printing All set."
  ;; Disabled:
  ;;  "Ask for a number i and show the ith goal, or show ancient goal.
  ;;If point is on a locked span, show the corresponding coq
  ;;output (i.e. for tactics: the goal after the tactic). Otherwise
  ;;ask for a number i and show the ith current goal. With non-nil
  ;;prefix argument and not on the locked span, show the goal with
  ;;flag Printing All set."
  (interactive "P")
  ;; Disabling this because we don't ask for a goal after every state, which would be
  ;; expensive. If Coq offered a way to query for the goal at a state id, we could query
  ;; for that
  (if (proof-in-locked-region-p)
      (message "Can't ask for non-current goal")
    (if with-printing-all
        (coq-queries-ask-show-all "Show goal number" "Show")
      (coq-queries-ask "Show goal number" "Show" t))))

(defun coq-Show-with-implicits ()
  "Ask for a number i and show the ith goal."
  (interactive)
  (coq-queries-ask-show-implicits "Show goal number" "Show"))

(defun coq-Show-with-all ()
  "Ask for a number i and show the ith goal."
  (interactive)
  (coq-queries-ask-show-all "Show goal number" "Show"))

;; Check
(cl-eval-when (compile)
           (defvar coq-auto-adapt-printing-width nil)); defpacustom

;; Since Printing Width is a synchronized option in coq (?) it is retored
;; silently to a previous value when retracting. So we reset the stored width
;; when retracting, so that it will be auto-adapted at the next command. Not
;; perfect: we have to forward one step to see the effect.

;; FIXME: hopefully this will eventually become a non synchronized option and
;; we can remove this.
(defun coq-set-auto-adapt-printing-width (&optional _val _symb); args are for :set compatibility
  "Function called when setting `auto-adapt-printing-width'"
  (if coq-auto-adapt-printing-width
      (progn
        (add-hook 'proof-assert-command-hook 'coq-adapt-printing-width)
        (add-hook 'proof-retract-command-hook 'coq-reset-printing-width))
    (remove-hook 'proof-assert-command-hook 'coq-adapt-printing-width)
    (remove-hook 'proof-retract-command-hook 'coq-reset-printing-width)))

(defpacustom auto-adapt-printing-width t
  "If non-nil, adapt automatically printing width of goals window.
Each timme the user sends abunch of commands to Coq, check if the
width of the goals window changed, and adapt coq printing width.
WARNING: If several windows are displaying the goals buffer, one
is chosen randomly. WARNING 2: when backtracking the printing
width is synchronized by coq (?!)."
  :type 'boolean
  :safe 'booleanp
  :group 'coq
  :eval (coq-set-auto-adapt-printing-width))


;; defpacustom fails to call :eval during inititialization, see trac #456
(coq-set-auto-adapt-printing-width)

;; this initiates auto adapt printing width at start, by reading the config
;; var. Let us put this at the end of hooks to have a chance to read local
;; variables first.
(add-hook 'coq-mode-hook 'coq-auto-adapt-printing-width t)

;; reset header line on retract
(add-hook 'proof-retract-command-hook 'coq-header-line-init)

(defvar coq-current-line-width nil
  "Current line width of the Coq printing width.
Its value will be updated whenever a command is sent if
necessary.")

;; Resetting the known printing width (for when we don't know it, for example
;; when retracting.
(defun coq-reset-printing-width ()
  (setq coq-current-line-width nil))

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
  (let (pol (proof-guess-3win-display-policy proof-three-window-mode-policy))
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
  "Sends a Set Printing Width command to coq to fit the response window's width.
A Show command is also issued if SHOW is non-nil, so that the
goal is redisplayed."
  (interactive)
  (let ((wdth (or width (coq-guess-goal-buffer-at-next-command))))
    ;; if no available width, or unchanged, do nothing
    (when (and wdth (not (equal wdth coq-current-line-width)))
      (let* ((print-width (1- wdth))
             (print-thunk (coq-queries-set-printing-width print-width)))
        (proof-invisible-command print-thunk)
        (when show
          (proof-invisible-command (lambda () (list (coq-xml-goal) nil)))))
      (setq coq-current-line-width wdth))))

(defun coq-adapt-printing-width-and-show(&optional _ width)
  (interactive)
  (coq-adapt-printing-width t width))

(defun coq-ask-adapt-printing-width-and-show ()
  (interactive)
  (let* ((deflt (coq-goals-window-width))
         (rd (read-number (format "Width (%S): " deflt) deflt)))
    (coq-adapt-printing-width t rd)))


(defvar coq-highlight-id-last-regexp nil)

(defun coq-highlight-id-in-goals (re)
  (with-current-buffer proof-goals-buffer
    (highlight-regexp re 'lazy-highlight)))

(defun coq-unhighlight-id-in-goals (re)
  (with-current-buffer proof-goals-buffer
    (unhighlight-regexp re)))

(defun coq-highlight-id-at-pt-in-goals ()
  (interactive)
  (let* ((id (coq-id-or-notation-at-point))
         (re (regexp-quote (or id ""))))
    (when coq-highlight-id-last-regexp
      (coq-unhighlight-id-in-goals coq-highlight-id-last-regexp))
       (coq-highlight-id-in-goals re)
       (setq coq-highlight-id-last-regexp re)))

;; Items on Other Queries menu

(proof-definvisible coq-print-hint (coq-queries-print-hint-thunk))
(proof-definvisible coq-show-tree (coq-queries-show-tree-thunk))
(proof-definvisible coq-show-proof (coq-queries-show-proof-thunk))
(proof-definvisible coq-show-conjectures (coq-queries-show-conjectures-thunk))
(proof-definvisible coq-show-intros (coq-queries-show-intros-thunk))

(proof-definvisible coq-set-printing-all (coq-queries-set-printing-all-thunk))
(proof-definvisible coq-unset-printing-all (coq-queries-unset-printing-all-thunk))
(proof-definvisible coq-set-printing-synth (coq-queries-set-printing-synth-thunk))
(proof-definvisible coq-unset-printing-synth (coq-queries-unset-printing-synth-thunk))
(proof-definvisible coq-set-printing-coercions (coq-queries-set-printing-coercions-thunk))
(proof-definvisible coq-unset-printing-coercions (coq-queries-unset-printing-coercions-thunk))
(proof-definvisible coq-set-printing-universes (coq-queries-set-printing-universes-thunk))
(proof-definvisible coq-unset-printing-universes (coq-queries-unset-printing-universes-thunk))
(proof-definvisible coq-set-printing-wildcards (coq-queries-set-printing-wildcard-thunk))
(proof-definvisible coq-unset-printing-wildcards (coq-queries-unset-printing-wildcard-thunk))
(proof-definvisible coq-pwd (coq-queries-pwd-thunk))

(defun coq-Compile ()
  "Compiles current buffer."
  (interactive)
  (let* ((n (buffer-name))
         (l (string-match ".v" n)))
    (compile (concat "make " (substring n 0 l) ".vo"))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Holes mode switch
;; TODO: have this plugged again when we have abbreviation without holes
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
;; explicitely the reverse in it sconfig file. I just want to change the
;; *default* value to nil.
(custom-set-default 'proof-output-tooltips nil)

;; This seems xemacs only code, remove?
(defconst coq-prettify-symbols-alist
  '(("not"	. ?¬)
    ;; ("/\\"	. ?∧)
    ("/\\"	. ?⋀)
    ;; ("\\/"	. ?∨)
    ("\\/"	. ?⋁)
    ;;("forall"	. ?∀)
    ("forall"	. ?Π)
    ("fun"	. ?λ)
    ("->"	. ?→)
    ("<-"	. ?←)
    ("=>"	. ?⇒)
    ;; ("~>"	. ?↝) ;; less desirable
    ;; ("-<"	. ?↢) ;; Paterson's arrow syntax
    ;; ("-<"	. ?⤙) ;; nicer but uncommon
    ("::"	. ?∷)
    ))


(defun coq-get-comment-region (pt)
  "Return a list of the forme (beg end) where beg,end is the comment region arount position PT.
Return nil if PT is not inside a comment"
  (save-excursion
    (goto-char pt)
    `(,(save-excursion (coq-find-comment-start))
      ,(save-excursion (coq-find-comment-end)))))

(defun coq-near-comment-region ()
  "Return a list of the forme (beg end) where beg,end is the comment region near position PT.
Return nil if PT is not near a comment.
Near here means PT is either inside or just aside of a comment."
  (save-excursion
    (cond
     ((coq-looking-at-comment)
      (coq-get-comment-region (point)))
     ((and (looking-back proof-script-comment-end)
           (save-excursion (forward-char -1) (coq-looking-at-comment)))
      (coq-get-comment-region (- (point) 1)))
     ((and (looking-at proof-script-comment-start)
           (save-excursion (forward-char) (coq-looking-at-comment)))
      (coq-get-comment-region (+ (point) 1))))))

(defun coq-fill-paragraph-function (_)
  "Coq mode specific fill-paragraph function. Fills only comment at point."
  (let ((reg (coq-near-comment-region)))
    (when reg
      (fill-region (car reg) (cadr reg))))
  t);; true to not fallback to standard fill function

;; TODO (but only for paragraphs in comments)
;; Should recognize coqdoc bullets, stars etc... Unplugged for now.
(defun coq-adaptive-fill-function ()
  (let ((reg (coq-near-comment-region)))
    (save-excursion
      (goto-char (car reg))
      (re-search-forward "\\((\\*+ ?\\)\\( *\\)")
      (let* ((cm-start (match-string 1))
             (cm-prefix (match-string 2)))
        (concat (make-string (length cm-start) ? ) cm-prefix)))))

(defun coq-interrupt-coq ()
  (when proof-server-process
    (let ((complete-p (tq-response-complete coq-server-transaction-queue)))
      (proof-server-clear-state)
      ;; resets completed flag
      ;; which is why we get its value first
      (tq-flush coq-server-transaction-queue) 
      (if complete-p
          ;; if Coq not working, stop active workers
          ;; locked region may not reflect what Coq has processed, so reset end
          (progn
            (message "Stopping Coq active workers")
            (coq-server-stop-active-workers))
        ;; on interrupt, get a fail-value, resulting in Edit_at
        (when (and proof-server-process (eq (process-status proof-server-process) 'run))
          (message "Sending SIGINT to Coq process")
          (interrupt-process proof-server-process t)
          (setq coq-server-retraction-on-interrupt t)))
      (let* ((current-span (coq-server--get-span-with-state-id coq-current-state-id))
             (end (if current-span (span-end current-span) 1)))
        (proof-set-queue-end end)
        (proof-set-locked-end end))
      (coq-header-line-update))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coq-mode-config ()
  ;; SMIE needs this.
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Coq error messages are thrown off by TAB chars.
  (set (make-local-variable 'indent-tabs-mode) nil)
  ;; Coq defninition never start by a parenthesis
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  ;; do not break lines in code when filling
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda () (not (nth 4 (syntax-ppss)))))
  ;; coq mode specific indentation function
  (set (make-local-variable 'fill-paragraph-function) 'coq-fill-paragraph-function)

  ;; TODO (but only for paragraphs in comments)
  ;; (set (make-local-variable 'paragraph-start)  "[ 	]*\\((\\**\\|$\\)")
  ;; (set (make-local-variable 'paragraph-separate) "\\**) *$\\|$")
  ;; (set (make-local-variable 'adaptive-fill-function) 'coq-adaptive-fill-function)

  ;; coq-mode colorize errors better than the generic mechanism
  (setq proof-script-color-error-messages nil)
  (setq proof-terminal-string ".")
  (setq proof-script-command-end-regexp coq-script-command-end-regexp)
  (setq proof-script-parse-function 'coq-script-parse-function)
  (setq proof-script-comment-start "(*")
  (setq proof-script-comment-end "*)")
  (setq proof-script-insert-newlines nil)
  (set (make-local-variable 'comment-start-skip)  "(\\*+ *")
  (set (make-local-variable 'comment-end-skip) " *\\*+)")
  (setq proof-unnamed-theorem-name "Unnamed_thm") ; Coq's default name

  (setq proof-assistant-home-page coq-www-home-page)

  (setq proof-prog-name coq-prog-name)
  (setq proof-guess-command-line 'coq-guess-command-line)
  (setq proof-prog-name-guess t)

  (setq proof-command-formatting-fun 'coq-format-command)
  (setq proof-server-interrupt-fun 'coq-interrupt-coq)
  (setq proof-server-response-complete-fun (lambda () (tq-response-complete coq-server-transaction-queue)))
  
  ;; We manage file saving via coq-compile-auto-save and for coq
  ;; it is not necessary to save files when starting a new buffer.
  (setq proof-query-file-save-when-activating-scripting nil)

  ;; Sent to proof engine
  (setq proof-showproof-command
        (lambda () (list (coq-xml-goal) nil)))
  (setq proof-goal-command "Goal %s. "
        proof-save-command "Save %s. ")
  ;; FIXME da: Does Coq have a help or about command?
  ;;	proof-info-command "Help"

  (setq proof-server-quit-cmd (lambda () (list (coq-xml-quit) nil)))
  (setq proof-context-command 'coq-get-context)
  
  (setq proof-goal-command-p 'coq-goal-command-p
        proof-find-and-forget-fn 'coq-server-find-and-forget
        pg-topterm-goalhyplit-fn 'coq-goal-hyp ; TODO not used?
        proof-state-preserving-p 'coq-state-preserving-p)

  ;; TODO REPLACE THIS?
  '(setq proof-query-identifier-command "Check %s.")
  ;;TODO: from v8.5 this wold be better:
  ;;(setq proof-query-identifier-command "About %s.")

  (setq proof-really-save-command-p 'coq-save-command-p ;pierre:deals with Proof <term>.
        proof-save-with-hole-regexp coq-save-with-hole-regexp
        proof-goal-with-hole-regexp coq-goal-with-hole-regexp
        proof-nested-undo-regexp coq-state-changing-commands-regexp
        proof-script-imenu-generic-expression coq-generic-expression)

  (when (fboundp 'smie-setup) ; always use smie, old indentation code removed
    (smie-setup coq-smie-grammar 'coq-smie-rules
                :forward-token 'coq-smie-forward-token
                :backward-token 'coq-smie-backward-token))

  ;; old indentation code.
  ;; (require 'coq-indent)
  ;; (setq
  ;;  ;; indentation is implemented in coq-indent.el
  ;;  indent-line-function 'coq-indent-line
  ;;  proof-indent-any-regexp      coq-indent-any-regexp
  ;;  proof-indent-open-regexp     coq-indent-open-regexp
  ;;  proof-indent-close-regexp    coq-indent-close-regexp)
  ;; (make-local-variable 'indent-region-function)
  ;; (setq indent-region-function 'coq-indent-region)
  
  
  ;; span menu
  (setq proof-script-span-context-menu-extensions 'coq-create-span-menu)

  '(setq proof-shell-start-silent-cmd "Set Silent. "
         proof-shell-stop-silent-cmd "Unset Silent. ")

  (coq-init-syntax-table)
  ;; we can cope with nested comments
  (set (make-local-variable 'comment-quote-nested) nil)

  ;; font-lock
  (setq proof-script-font-lock-keywords coq-font-lock-keywords-1)

  ;; FIXME: have abbreviation without holes
                                        ;(if coq-use-editing-holes (holes-mode 1))
  (holes-mode 1)

  (setq 
   proof-server-send-to-prover-fun 'coq-server-send-to-prover
   proof-server-make-command-thunk-fun 'coq-server-make-add-command-thunk
   proof-server-process-response-fun 'coq-server-process-response
   proof-server-init-cmd coq-server-init-cmds
   proof-server-retract-buffer-hook 'coq-reset-all-state
   proof-find-theorems-command 'coq-find-theorems
   proof-check-command 'coq-check-document)

  ;; prooftree config
  (setq
   proof-tree-configured t
   proof-tree-get-proof-info 'coq-proof-tree-get-proof-info
   proof-tree-find-begin-of-unfinished-proof
   'coq-find-begin-of-unfinished-proof)

  (proof-config-done)

  ;; outline
  (set (make-local-variable 'outline-regexp) coq-outline-regexp)
  (set (make-local-variable 'outline-heading-end-regexp)
       coq-outline-heading-end-regexp)

  ;; tags
  (if (file-exists-p coq-tags)
      (set (make-local-variable 'tags-table-list)
           (cons coq-tags tags-table-list)))
  
  (set (make-local-variable 'blink-matching-paren-dont-ignore-comments) t)

  (when coq-may-use-prettify
    (set (make-local-variable 'prettify-symbols-alist)
         coq-prettify-symbols-alist))

  (setq proof-cannot-reopen-processed-files nil)

  (add-hook 'proof-activate-scripting-hook 'proof-cd-sync nil t)

  (proof-eval-when-ready-for-assistant
   (easy-menu-define proof-goals-mode-aux-menu
     proof-goals-mode-map
     "Menu for Proof General goals buffer."
     (cons "Coq" coq-other-buffers-menu-entries)))

  (proof-eval-when-ready-for-assistant
   (easy-menu-define proof-goals-mode-aux-menu
     proof-response-mode-map
     "Menu for Proof General response buffer."
     (cons "Coq" coq-other-buffers-menu-entries))))

;; formatter for user commands entered in minibuffer
(defun coq-format-command (cmd)
  (lambda ()
    (list (coq-xml-query-item cmd)
          nil)))

(defun coq-goals-mode-config ()
  (setq pg-goals-change-goal "Show %s . ")
  (setq pg-goals-error-regexp coq-error-regexp)
  (coq-init-syntax-table)
  (setq proof-goals-font-lock-keywords coq-goals-font-lock-keywords)
  (proof-goals-config-done))

(defun coq-response-config ()
  (coq-init-syntax-table)
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
;;  :setting ("Focus. " . "Unfocus. "))


(defpacustom hide-additional-subgoals nil
  "Show all subgoals if off, show only the current goal if on."
  :type 'boolean
  :safe 'booleanp
  :eval (coq-hide-additional-subgoals-switch))


                                        ;
;;; FIXME: to handle "printing all" properly, we should change the state
;;; of the variables that also depend on it.
;;; da:

;;; pc: removed it and others of the same kind. Put an "option" menu instead,
;;; with no state variable. To have the state we should use coq command that
;;; output the value of the variables.
                                        ;(defpacustom print-fully-explicit nil
                                        ;  "Print fully explicit terms."
                                        ;  :type 'boolean
                                        ;  :setting ("Set Printing All. " . "Unset Printing All. "))
                                        ;

(defpacustom printing-depth 50
  "Depth of pretty printer formatting, beyond which an ellipsis (...) is displayed."
  :type 'integer
  :setting (lambda (depth) 
             (coq-queries-set-printing-depth depth)))

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
  (let* ((info (coq-current-proof-info)))
    (list (nth 0 info)    ; state number
          (nth 3 info)))) ; name of current proof

(defun coq-extract-instantiated-existentials (start end)
  "Coq specific function for `proof-tree-extract-instantiated-existentials'.
Returns the list of currently instantiated existential variables."
  (proof-tree-extract-list
   start end
   coq-proof-tree-existentials-state-start-regexp
   coq-proof-tree-existentials-state-end-regexp
   coq-proof-tree-instantiated-existential-regexp))

(defun coq-show-sequent-command (sequent-id)
  "Coq specific function for `proof-tree-show-sequent-command'."
  (format "Show Goal \"%s\"." sequent-id))

(defun coq-proof-tree-get-new-subgoals ()
  "Check for new subgoals and issue appropriate Show commands.
This is a hook function for `proof-tree-urgent-action-hook'. This
function examines the current goal output and searches for new
unknown subgoals. Those subgoals have been generated by the last
proof command and we must send their complete sequent text
eventually to prooftree. Because subgoals may change with
the next proof command, we must execute the additionally needed
Show commands before the next real proof command.

The ID's of the open goals are checked with
`proof-tree-sequent-hash' in order to find out if they are new.
For any new goal an appropriate Show Goal command with a
'proof-tree-show-subgoal flag is inserted into
`proof-action-list'. Then, in the normal delayed output
processing, the sequent text is send to prooftree as a sequent
update (see `proof-tree-update-sequent') and the ID of the
sequent is registered as known in `proof-tree-sequent-hash'.

Searching for new subgoals must only be done when the proof is
not finished, because Coq 8.5 lists open existential variables
as (new) open subgoals. For this test we assume that
`proof-marker' has not yet been moved.

The not yet delayed output is in the region
\[proof-shell-delayed-output-start, proof-shell-delayed-output-end]."
  ;; (message "CPTGNS start %s end %s"
  ;;          proof-shell-delayed-output-start
  ;;          proof-shell-delayed-output-end)
  (with-current-buffer proof-shell-buffer
    (let ((start proof-shell-delayed-output-start)
          (end proof-shell-delayed-output-end))
      ;; The message "All the remaining goals are on the shelf" is processed as
      ;; urgent message and is therefore before
      ;; proof-shell-delayed-output-start. We therefore need to go back to
      ;; proof-marker.
      (goto-char proof-marker)
      (unless (proof-re-search-forward
               coq-proof-tree-branch-finished-regexp end t)
        (goto-char start)
        (while (proof-re-search-forward
                coq-proof-tree-additional-subgoal-ID-regexp end t)
          (let ((subgoal-id (match-string-no-properties 1)))
            (unless (gethash subgoal-id proof-tree-sequent-hash)
              (message "CPTGNS new sequent %s found" subgoal-id)
              (setq proof-action-list
                    (cons (proof-shell-action-list-item
                           (coq-show-sequent-command subgoal-id)
                           (proof-tree-make-show-goal-callback (car proof-info))
                           '(no-goals-display
                             no-response-display
                             proof-tree-show-subgoal))
                          proof-action-list)))))))))

(add-hook 'proof-tree-urgent-action-hook 'coq-proof-tree-get-new-subgoals)


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
    ;; go backward as long as the state-id property in the span is greater or
    ;; equal than state
    (while (<= state (span-property span-cur 'state-id))
      (setq span-res span-cur)
      (setq span-cur (span-at (1- (span-start span-cur)) 'type)))
    (span-start span-res)))


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
        (setq string (concat coq--time-prefix string))))))

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
  (while (re-search-forward "\(\w+[^\w]\)" nil t)
    (replace-match "\372\200\373\\1\374" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Context-sensitive in-span menu additions
;;

(defun coq-create-span-menu (span idiom _)
  (if (eq idiom 'proof)
      (let ((thm (span-property span 'name)))
        (list (vector
               "Check" ; useful?
               `(proof-invisible-command
                 (lambda ()
                   (list 
                    (coq-xml-query-item 
                     ,(format "Check %s." thm))
                    nil)))
               (vector
                "Print"
                `(proof-invisible-command
                  (lambda ()
                    (list 
                     (coq-xml-query-item 
                      ,(format "Print %s." thm))
                     nil)))))))))

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
  (let* ((file-list-list (mapcar 'directory-files l))
         (file-list (apply 'append file-list-list))
         (filtered-list (cl-remove-if-not 'coq-postfix-.v-p file-list)))
    filtered-list))

(defun coq-remove-dot-v-extension (s)
  (substring s 0 -2))

(defun coq-load-path-to-paths (ldpth)
  (if (listp ldpth) (car ldpth) ldpth))

(defun coq-build-accessible-modules-list ()
  (let* ((pth (or coq-load-path '(".")))
         (cleanpth (mapcar 'coq-load-path-to-paths pth))
         (existingpth (cl-remove-if-not 'file-exists-p cleanpth))
         (file-list (coq-directories-files existingpth)))
    (mapcar 'coq-remove-dot-v-extension file-list)))

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

(defvar coq-keywords-accepting-as-regex (regexp-opt '("induction" "destruct" "inversion" "injection")))

;; destruct foo., where foo is a name.
(defvar coq-auto-as-hack-hyp-name-regex 
  (concat "\\(" "induction\\|destruct" "\\)\\s-+\\(" coq-id-shy "\\)\\s-*\\.")
  "Regexp of commands needing a hack to generate correct \"as\" close.
tacitcs like destruct and induction reuse hypothesis names by
default, which makes the detection of new hypothesis incorrect.
the hack consists in adding the \"!\" modifier on the argument
destructed, so that it is left in the goal and the name cannot be
reused. We also had a \"clear\" at the end of the tactic so that
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
        (proof-server-invisible-cmd-handle-result
         (lambda ()
           (list
            (coq-xml-query-item newcmd)
            nil))
         (lambda (response _call _span)
           (let ((result (coq-queries-get-message-string response)))
             (if (null result) ;; TODO bug? in 8.5 XML protocol, don't get any string back
                 (coq-queries-process-response response nil nil) ; show error if no valid response
               (unless (coq-tactic-already-has-an-as-close newcmd) ;; FIXME ???
                 (with-current-buffer proof-script-buffer
                   (save-excursion
                     ;; TODO: look for eqn:XX and go before it.
                     ;; Go just before the last "."
                     (goto-char (proof-unprocessed-begin))
                     (coq-script-parse-cmdend-forward)
                     (coq-script-parse-cmdend-backward)
                     (insert (concat " as [" result "]")))))))))))))

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
  (proof-ready-prover)
  (let* (cmd)
    (setq cmd (read-string "Build match for type: "))
    (proof-invisible-cmd-handle-result
     (lambda ()
       (list (coq-xml-query-item (concat "Show Match " cmd " ."))
             nil))
     (lambda (response _call _span)
       (let* ((the-match (coq-queries-get-message-string response))
              (match (replace-regexp-in-string "=> \n" "=> #\n" the-match)))
         ;; if error, it will be displayed in response buffer (see def of
         ;; proof-invisible-cmd-get-result), otherwise:
         (unless (proof-string-match coq-error-regexp match)
           (with-current-buffer proof-script-buffer
             (let ((start (point)))
               (insert match)
               (indent-region start (point) nil)
               (let ((n (holes-replace-string-by-holes-backward start)))
                 (cl-case n
                          (0 nil)				; no hole, stay here.
                          (1
                           (goto-char start)
                           (holes-set-point-next-hole-destroy)) ; if only one hole, go to it.
                          (t
                           (goto-char start)
                           (message
                            (substitute-command-keys
                             "\\[holes-set-point-next-hole-destroy] to jump to active hole.  \\[holes-short-doc] to see holes doc.")))))))))))))

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

(defun coq-query (show-all)
  "Ask for a query, with completion, and send to Coq."
  (interactive "P")
  (let* ((query (coq-build-command-from-db coq-queries-commands-db "which Query?"))
         (thunk (lambda () (list (coq-xml-query-item (concat query " ."))))))
    (if show-all
        (coq-queries-ask-set-unset
         thunk
         (coq-queries-set-printing-all)
         (coq-queries-unset-printing-all)
         '("Printing All"))
      (proof-invisible-command thunk))))

;; Header line mouse handler
(when coq-use-header-line
  (global-set-key [header-line mouse-1] 'coq-header-line-mouse-handler))

;; Insertion commands
(define-key coq-keymap [(control ?i)] 'coq-insert-intros)
(define-key coq-keymap [(control ?m)] 'coq-insert-match)
(define-key coq-keymap [(control ?()] 'coq-insert-section-or-module)
  (define-key coq-keymap [(control ?))] 'coq-end-Section)
(define-key coq-keymap [(control ?t)] 'coq-insert-tactic)
(define-key coq-keymap [?t] 'coq-insert-tactical)
(define-key coq-keymap [?!] 'coq-insert-solve-tactic) ; will work in tty
(define-key coq-keymap [(control ?\s)] 'coq-insert-term)
(define-key coq-keymap [(control return)] 'coq-insert-command)
(define-key coq-keymap [(control ?q)] 'coq-query)
(define-key coq-keymap [(control ?r)] 'coq-insert-requires)
;; [ for "as [xxx]" is easy to remember, ccontrol-[ would be better but hard to type on french keyboards
;; anyway company-coq should provide an "as!<TAB>". 
(define-key coq-keymap [(?\[)] 'coq-insert-as-in-next-command) ;; not for goal/response buffer?

;; Query commands
(define-key coq-keymap [(control ?s)] 'coq-Show)
(define-key coq-keymap [?r] 'proof-store-response-win)
(define-key coq-keymap [?g] 'proof-store-goals-win)
(define-key coq-keymap [(control ?o)] 'coq-SearchIsos)
(define-key coq-keymap [(control ?p)] 'coq-Print)
(define-key coq-keymap [(control ?b)] 'coq-About)
(define-key coq-keymap [(control ?c)] 'coq-Check)
(define-key coq-keymap [?h] 'coq-PrintHint)
(define-key coq-keymap [(control ?l)] 'coq-LocateConstant)
(define-key coq-keymap [(control ?n)] 'coq-LocateNotation)
(define-key coq-keymap [(control ?w)] 'coq-ask-adapt-printing-width-and-show)
(define-key coq-keymap [(control ?d)] 'coq-check-document)

;;(proof-eval-when-ready-for-assistant
;; (define-key ??? [(control c) (control a)] (proof-ass keymap)))

;;(proof-eval-when-ready-for-assistant
;; (define-key ??? [(control c) (control a)] (proof-ass keymap)))

(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?c)] 'coq-Check)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?d)] 'coq-check-document)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?p)] 'coq-Print)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?o)] 'coq-SearchIsos)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?b)] 'coq-About)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?s)] 'coq-Show)
(define-key coq-goals-mode-map [(control ?c)(control ?a)?r] 'proof-store-response-win)
(define-key coq-goals-mode-map [(control ?c)(control ?a)?g] 'proof-store-goals-win)
(define-key coq-goals-mode-map [(control ?c)(control ?a)?h] 'coq-PrintHint)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?q)] 'coq-query)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?w)] 'coq-ask-adapt-printing-width-and-show)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?l)] 'coq-LocateConstant)
(define-key coq-goals-mode-map [(control ?c)(control ?a)(control ?n)] 'coq-LocateNotation)


(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?c)] 'coq-Check)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?d)] 'coq-check-document)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?p)] 'coq-Print)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?o)] 'coq-SearchIsos)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?b)] 'coq-About)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?s)] 'coq-Show)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?r)] 'proof-store-response-win)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?g)] 'proof-store-goals-win)
(define-key coq-response-mode-map [(control ?c)(control ?a)?h] 'coq-PrintHint)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?q)] 'coq-query)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?w)] 'coq-ask-adapt-printing-width-and-show)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?l)] 'coq-LocateConstant)
(define-key coq-response-mode-map [(control ?c)(control ?a)(control ?n)] 'coq-LocateNotation)

(when coq-remap-mouse-1
  (define-key proof-mode-map [(control down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-mode-map [(shift down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-mode-map [(control mouse-1)] '(lambda () (interactive)))
  (define-key proof-mode-map [(shift mouse-1)] '(lambda () (interactive)))
  (define-key proof-mode-map [(control shift down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-mode-map [(control shift mouse-1)] '(lambda () (interactive)))

  (define-key proof-response-mode-map [(control down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-response-mode-map [(shift down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-response-mode-map [(control mouse-1)] '(lambda () (interactive)))
  (define-key proof-response-mode-map [(shift mouse-1)] '(lambda () (interactive)))
  (define-key proof-response-mode-map [(control shift down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-response-mode-map [(control shift mouse-1)] '(lambda () (interactive)))

  (define-key proof-goals-mode-map [(control down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-goals-mode-map [(shift down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-goals-mode-map [(control mouse-1)] '(lambda () (interactive)))
  (define-key proof-goals-mode-map [(shift mouse-1)] '(lambda () (interactive)))
  (define-key proof-goals-mode-map [(control shift down-mouse-1)] 'coq-id-under-mouse-query)
  (define-key proof-goals-mode-map [(control shift mouse-1)] '(lambda () (interactive))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; error handling
;;;;;;;;;;;;;;;;;;;;;;;;

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
                (search-forward-regexp "subgoal 2\\|\\'")
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
                     (string-to-number (coq-first-word-before "focused\\|subgoal"))))
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

;;; DOUBLE HIT ELECTRIC TERMINATOR
;; Trying to have double hit on colon behave like electric terminator. The "."
;; is used for records and modules qualified notatiohns, so electric terminator
;; is not pertinent.

;; TODO: make this a minor mode with something in the modeline, like in
;; pg-user.el for electric-terminator.
;; TODO: Have the same for other commands, but with insertion at all.

(defcustom coq-double-hit-enable nil
  "* Experimental: Whether or not double hit should be enabled in coq mode.
A double hit is performed by pressing twice a key quickly. If
this variable is not nil, then 1) it means that electric
terminator is off and 2) a double hit on the terminator act as
the usual electric terminator. See `proof-electric-terminator'.
"
  :type 'boolean
  :set 'proof-set-value
  :group 'proof-user-options)


(defvar coq-double-hit-hot-key "."
  "The key used for double hit electric terminator. By default this
is the coq terminator \".\" key. For example one can do this:

(setq coq-double-hit-hot-key (kbd \";\"))

                                  to use semi-colon instead (on french keyboard, it is the same key
                                                                as \".\" but without shift.")

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
  ;; (if (not coq-double-hit-enable)
  ;;     (define-key coq-mode-map (kbd coq-double-hit-hot-key) coq-double-hit-hot-keybinding)
  ;;    (setq coq-double-hit-hot-keybinding (key-binding coq-double-hit-hot-key))
  ;;    (define-key coq-mode-map (kbd coq-double-hit-hot-key) 'coq-terminator-insert))
  )



;;(define-key coq-mode-map coq-double-hit-hot-key 'coq-terminator-insert)

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
  "the timer used to watch for double hits.")

(defvar coq-double-hit-hot nil
  "The variable telling that a double hit is still possible.")



(defun coq-unset-double-hit-hot ()
  "Disable timer `coq-double-hit-timer' and set it to nil. Shut
off the current double hit if any. This function is supposed to
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
                          nil 'coq-unset-double-hit-hot))))

(defun coq-terminator-insert (&optional count)
  "A wrapper on `proof-electric-terminator' to accept double hits instead if enabled.
If by accident `proof-electric-terminator-enable' and `coq-double-hit-enable'
are non-nil at the same time, this gives priority to the former."
  (interactive)
  (if (and (not proof-electric-terminator-enable)
           coq-double-hit-enable (null count))
      (coq-colon-self-insert)
    ;; otherwise call this, which checks proof-electric-terminator-enable
    (proof-electric-terminator count)))

(put 'coq-terminator-insert 'delete-selection t)

;; Setting the new mapping for terminator, overrides the following in proof-script:
;; (define-key proof-mode-map (vector (aref proof-terminal-string 0)) 'proof-electric-terminator)

                                        ;(define-key proof-mode-map (kbd coq-double-hit-hot-key) 'coq-terminator-insert)
(define-key coq-mode-map (kbd ".") 'coq-terminator-insert)
                                        ;(define-key coq-mode-map (kbd ";") 'coq-terminator-insert) ; for french keyboards

;; Activation of ML4PG functionality
(declare-function ml4pg-select-mode "ml4pg") ;; Avoids copilation warnings

(defun coq-activate-ml4pg ()
  (let ((filename (cl-concatenate 'string proof-home-directory "contrib/ML4PG/ml4pg.el")))
    (when (file-exists-p filename) (load-file filename) (ml4pg-select-mode))))

;;;;;;;;;;;;;;

;; This was done in coq-compile-common, but it is actually a good idea even
;; when "compile when require" is off. When switching scripting buffer, let us
;; restart the coq shell process, so that it applies local coqtop options. 
(add-hook 'proof-deactivate-scripting-hook
          'coq-switch-buffer-kill-proof-server ;; this function is in coq-compile-common
          t)


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
