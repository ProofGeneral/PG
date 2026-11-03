;;; coq-queries.el --- Queries to Coq: About, Check, Search etc.  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2026  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; The query commands: they ask for an identifier or a term, guessed
;; from point, and send a query about it to Coq, like About, Check,
;; Print, Search or Locate.
;;
;; They are common to all Coq backends: they run the query with
;; `coq-run-query', which calls `coq-query-function', set by the
;; backend.  Their keys are in `coq-mode-map'.

;;; Code:

(require 'cl-lib)                       ; `cl-pushnew'
(require 'subr-x)                       ; `string-trim-right'
(require 'coq-syntax)                   ; `coq-with-altered-syntax-table'
(require 'coq-db)                       ; `coq-build-command-from-db'

(defvar proof-minibuffer-history)
(defvar coq-highlight-hyps-cited-in-response)
(defvar coq-one-command-per-line)
(declare-function coq-highlight-hyps-cited-in-response "coq")
(declare-function coq-terminator-insert "coq-mode")

(defvar-local coq-query-function nil
  "Function running a query, set by each Coq backend.
It is called with the arguments COMMAND FLAGS THEN.  COMMAND is the
query, without its final dot, for instance \"About nat\".  FLAGS is a
list of commands setting boolean printing flags for this query only,
for instance (\"Set Printing All\"), the backend undoes them after the
query.  THEN is nil or a function, called without argument once the
answer is displayed.")

(defun coq-run-query (command &optional flags then)
  "Run the query COMMAND with printing FLAGS, then call THEN.
See `coq-query-function' for the arguments."
  (unless coq-query-function
    (user-error "Coq: no backend to run queries in this buffer"))
  (funcall coq-query-function command flags then))

(defvar-local coq-query-result-function nil
  "Function running a query and returning its answer, set by each backend.
It is called with the argument COMMAND, the query without its final
dot, for instance \"Show Intros\".  It waits for the answer and returns
it as a string, without displaying it, or nil if Coq answers an error.")

(defun coq-query-result (command)
  "Run the query COMMAND and return its answer, nil on an error.
See `coq-query-result-function'."
  (unless coq-query-result-function
    (user-error "Coq: no backend to run queries in this buffer"))
  (funcall coq-query-result-function command))

(defcustom coq-query-flags
  '(("All" . "Set Printing All")
    ("Implicit" . "Set Printing Implicit")
    ("Coercions" . "Set Printing Coercions")
    ("Parentheses" . "Set Printing Parentheses")
    ("No Notations" . "Unset Printing Notations")
    ("Universes" . "Set Printing Universes"))
  "Printing flags that a query can use, by name.
With \\[universal-argument] \\[universal-argument], query commands ask
which ones to use, see `coq-query-flags-from-prefix'.  Each value is the
Coq command setting a boolean flag, for this query only."
  :type '(alist :key-type (string :tag "Name")
                :value-type (string :tag "Coq command"))
  :group 'coq)

(defun coq-query-flags-from-prefix (arg)
  "Return the printing flags of a query with prefix argument ARG.
None without prefix argument.  With \\[universal-argument] \\[universal-argument] or more,
ask which ones among `coq-query-flags'.  With any other prefix
argument, for instance \\[universal-argument], Set Printing All."
  (cond
   ((null arg) nil)
   ((and (consp arg) (>= (car arg) 16))
    (mapcar (lambda (name) (cdr (assoc name coq-query-flags)))
            (completing-read-multiple "Printing flags: " coq-query-flags
                                      nil t)))
   (t (list "Set Printing All"))))

(defun coq--query-prompt (prompt flags)
  "PROMPT for a query, mentioning FLAGS if any."
  (if flags (format "%s [%s]" prompt (string-join flags ", ")) prompt))

(defconst notation-print-kinds-table
  '(("Print Scope(s)" 0) ("Print Visibility" 1))
  "Enumerates the different kinds of notation information one can ask to Coq.")

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
      (coq-run-query "Print Scopes"))
     (t
      (coq-run-query (format "%s %s" mods s))))))

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
  "Return a string made of punctuation chars found immediately before position POS."
  (let ((res nil)
        (currpos pos))
    (while (coq-is-symbol-or-punct (char-before currpos))
      (setq res (concat (char-to-string (char-before currpos)) res))
      (setq currpos (- currpos 1)))
    res))


(defun coq-grab-punctuation-right (pos)
  "Return a string made of punctuation chars found immediately after position POS."
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

(defvar coq-mouse-query-keymaps nil
  "The keymaps where `coq-remap-mouse-1' binds the queries at mouse click.
See `coq-mouse-query-add-keymap'.")

(defconst coq--mouse-query-keys
  '([(control down-mouse-1)] [(shift down-mouse-1)]
    [(control shift down-mouse-1)] [(control meta down-mouse-1)]
    [(meta shift down-mouse-1)])
  "The keys of the queries at mouse click, see `coq-id-under-mouse-query'.")

(defconst coq--mouse-query-ignored-keys
  '([(control mouse-1)] [(shift mouse-1)] [(control shift mouse-1)]
    [(meta shift mouse-1)])
  "The up events of `coq--mouse-query-keys', ignored.")

(defcustom coq-remap-mouse-1 nil
  "Whether Coq mode should remap mouse button 1 to Coq queries.
See `coq-id-under-mouse-query'.  This is for the script buffer and, in
the backends that support it, the goals and response buffers.

This overrides the default global binding of (control mouse-1) and
\(shift mouse-1) (buffers and faces menus).  Hence it is nil by
default."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (mapc #'coq--mouse-query-update coq-mouse-query-keymaps))
  :group 'coq)

(defun coq--mouse-query-update (map)
  "Bind or unbind the queries at mouse click in MAP.
Bind them if `coq-remap-mouse-1' is non-nil, unbind them otherwise."
  (dolist (key coq--mouse-query-keys)
    (define-key map key (and coq-remap-mouse-1 #'coq-id-under-mouse-query)))
  (dolist (key coq--mouse-query-ignored-keys)
    (define-key map key (and coq-remap-mouse-1 #'ignore))))

(defun coq-mouse-query-add-keymap (map)
  "Add MAP to `coq-mouse-query-keymaps' and update its bindings.
`coq-mode-map' is there, each backend adds the keymaps of its goals
and response buffers."
  (cl-pushnew map coq-mouse-query-keymaps :test #'eq)
  (coq--mouse-query-update map))


;; On a mouse event, try to query the id at point clicked.
(defun coq-id-under-mouse-query (event)
  "Query the prover about the identifier or notation near mouse click EVENT.
This is mapped to control/shift mouse-1, unless coq-remap-mouse-1
is nil (which is the default).

More precisely:
- ctrl click issues a Print query
- shift issues a Check query
- ctrl+shift click issued a Check query
- adding meta (alt) to the modifiers perform the same query but with \"Set
  Printing All\" set
"
  (interactive "e")
  (save-selected-window
    (save-excursion
      (mouse-set-point event)
      (let* ((id (coq-id-at-point))
             (notat (coq-notation-at-position (point)))
             (modifs (event-modifiers event))
             (shft (member 'shift modifs))
             (ctrl (member 'control modifs))
             (mta (member 'meta modifs))
             (c (cond
                 ((and (not notat) (not id))
                  (error "Can't detect anything meaningful under mouse"))
                 ((and notat (not id)) "Locate")
                 ((and ctrl shft) "Check")
                 (shft "About")
                 (ctrl "Print")))
             ;; Notation need to be surrounded by ""
             (cmd (format (concat c " %s") (or id (concat "\"" notat "\"")))))
        ;; meta means: Set printing all for the command
        (coq-run-query cmd (and mta '("Set Printing All")))))))

(defun coq-guess-or-ask-for-string (s &optional dontguess)
  "Asks for a Coq identifier with message S.
If DONTGUESS is non nil, propose a default value as follows:

If region is active, propose its content as default value.

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


(defun coq-ask-do (ask do &optional dontguess postformatcmd wait flags)
  "Ask for an argument with prompt ASK and run the query DO on it.
The argument is guessed from point or region, unless DONTGUESS, see
`coq-guess-or-ask-for-string'.  If non-nil, POSTFORMATCMD formats the
argument.  WAIT may be a function, called once the answer is
displayed, any other non-nil value waits for the answer.  FLAGS are
printing flags for this query only, see `coq-query-function'."
  (let ((postform (or postformatcmd #'identity))
        (arg (coq-guess-or-ask-for-string (coq--query-prompt ask flags)
                                          dontguess)))
    (coq-run-query (concat do " " (funcall postform arg))
                   flags
                   (cond ((functionp wait) wait)
                         (wait #'ignore)))))


(defun coq-ask-do-set-unset (ask do setcmd _unsetcmd
                                 &optional dontguess postformatcmd _tescmd)
  "Ask for an argument with prompt ASK and run the query DO on it.
Run it with the printing flag SETCMD, for instance \"Set Printing All\",
for this query only: the backend undoes it, UNSETCMD is ignored.  See
`coq-ask-do' for DONTGUESS and POSTFORMATCMD."
  (coq-ask-do ask do dontguess postformatcmd nil (list setcmd)))


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

(defun coq-Print (arg)
  "Ask for an ident and print the corresponding term.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do "Print" "Print" nil nil nil (coq-query-flags-from-prefix arg)))

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

(defun coq-About (arg)
  "Ask for an ident and print information on it.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do "About" "About" nil nil nil (coq-query-flags-from-prefix arg)))

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

(defun coq-Pwd ()
  "Display the current Coq working directory."
  (interactive)
  (coq-run-query "Pwd"))

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

(defun coq-Check (arg)
  "Ask for a term and print its type.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do "Check" "Check" nil nil nil (coq-query-flags-from-prefix arg)))

(defun coq-Check-show-implicits ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-implicits "Check" "Check"))

(defun coq-Check-show-all ()
  "Ask for a term and print its type."
  (interactive)
  (coq-ask-do-show-all "Check" "Check"))

(defun coq-query (arg)
  "Ask for a query, with completion, and send to Coq.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (let* ((flags (coq-query-flags-from-prefix arg))
         (q (coq-build-command-from-db coq-queries-commands-db
                                       (coq--query-prompt "which Query?"
                                                          flags))))
    (coq-run-query (coq-remove-trailing-dot (string-trim-right q)) flags)))

(defun coq-Search (&optional arg)
  "Ask for a pattern and search the lemmas matching it.
With \\[universal-argument], with Printing All.  With
\\[universal-argument] \\[universal-argument], ask which printing flags to use, see
`coq-query-flags-from-prefix'."
  (interactive "P")
  (coq-ask-do
   ;; TODO: use [Add Search Blacklist "foo"] to exclude optionaly some patterns:
   ;;  "_ind" "_rec" "R_" "_equation"
   "Search (ex: \"eq_\" eq -bool)"
   "Search"
   nil nil #'coq-search-done (coq-query-flags-from-prefix arg)))

(defun coq-search-done ()
  "Highlight the hypotheses cited by the answer of a Search, if wanted.
See `coq-highlight-hyps-cited-in-response'."
  (if (and coq-highlight-hyps-cited-in-response
           (fboundp 'coq-highlight-hyps-cited-in-response))
      (progn
        (coq-highlight-hyps-cited-in-response)
        (message "M-x coq-unhighlight-selected-hyps to remove highlighting, [Coq/Settings/Search Blacklist] to change blacklisting."))
    (message "[Coq/Settings/Search Blacklist] to change blacklisting.")))

;; Items of the menu Show, see coq-abbrev.el

(defun coq-PrintHint ()
  "Print the hints that apply to the current goal."
  (interactive)
  (coq-run-query "Print Hint"))

(defun coq-show-tree ()
  "Show the proof tree."
  (interactive)
  (coq-run-query "Show Tree"))

(defun coq-show-proof ()
  "Show the proof term built so far."
  (interactive)
  (coq-run-query "Show Proof"))

(defun coq-show-conjectures ()
  "Show the names of the current conjectures."
  (interactive)
  (coq-run-query "Show Conjectures"))

(defun coq-show-intros ()
  "Show the names that intros would give.
See `coq-insert-intros' to insert them."
  (interactive)
  (coq-run-query "Show Intros"))

;; Insertions built from the answer of a query

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
  (let* ((output (or (coq-query-result "Show Intros") "")))
    (indent-region (point)
                   (progn (insert (coq--format-intros output))
                          (save-excursion
                            ;; t if the backend does not define it
                            (insert (if (or (not (boundp 'coq-one-command-per-line))
                                            coq-one-command-per-line)
                                        "\n" " "))
                            (point))))
    ;; The electric terminator moves the point in all sorts of strange
    ;; ways, so we run it last
    (let ((last-command-event ?.)) ;; Insert a dot
      (coq-terminator-insert))))

(defun coq-replace-branch-with-yas-holes (s)
  "Replace occurrences of RE in by REPi in S. i is incremented at each replacement."
  (let ((res s) (i 2))
    (setq res (replace-regexp-in-string "\\`match #" "match $1" res))
    (while (string-match "=> *\n" res nil t)
      (setq res (replace-regexp-in-string ;; the regexp is made to match only
                                          ;; the fiorst occurrence
                 "\\(?1:=> *\n\\)\\(.\\|\n\\)*\\'"
                 (concat "=> $" (int-to-string i) "\n")
                 res nil nil 1))
      (setq i (+ 1 i)))
    res))

(defun coq-get-goal-names (show-existentials-output)
  "Return the list of goal names by parsing SHOW-EXISTENTIALS-OUTPUT.

Goals that are marked as \"only printing\" are ignored."
  (let ((goal-regex "Existential [0-9]+ =\\s-+\\?\\([^ ]+\\) :\\s-+\\[[^]]*]\\( (\\(?:shelved; \\)?only printing)\\)?")
        (pos 0)
        (matches '()))
    (while (string-match goal-regex show-existentials-output pos)
      (let ((goal-name (match-string 1 show-existentials-output))
            (only-printing (match-string 2 show-existentials-output)))
        (unless only-printing
          (push goal-name matches))
        (setq pos (match-end 0))))
    (nreverse matches)))

(defun coq-insert-named-goal-selectors ()
  "Insert named goal selectors for the open goals."
  (interactive)
  (let ((output (coq-query-result "Show Existentials")))
    (when output
      (let* ((goal-names (coq-get-goal-names output))
             ;; NOTE: Adding braces would be great, but it messes up indentation.
             (format-selector (lambda (name) (format "[%s]: #." name)))
             (goal-selectors (cl-mapcar format-selector goal-names))
             (snippet (string-join goal-selectors "\n")))
        (if (equal goal-selectors nil)
            (error "Couldn't find any named goals")
          (coq-insert-template snippet))))))

(defun coq-insert-match ()
  "Insert a match expression from a type name by Show Match.
Based on idea mentioned in Coq reference manual.
Also insert holes at insertion positions."
  (interactive)
  (let* ((cmd))
    (setq cmd (read-string "Build match for type: "))
    (let ((thematch (coq-query-result (concat "Show Match " cmd))))
      ;; nil on an error
      (when thematch
        (if (and coq-use-yasnippet (fboundp 'yas-expand-snippet))
            (yas-expand-snippet (coq-replace-branch-with-yas-holes thematch))
          (let ((start (point)))
            (insert thematch)
            (indent-region start (point) nil)
            (goto-char start)
            (search-forward "#")
            (delete-char -1)))))))

(provide 'coq-queries)

;;; coq-queries.el ends here
