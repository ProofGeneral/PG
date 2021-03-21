;;; coq-smie.el --- SMIE lexer, grammar, and indent rules for Coq  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2018  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Authors: Pierre Courtieu
;;          Stefan Monnier
;; Maintainer: Pierre Courtieu <Pierre.Courtieu@cnam.fr>

;; License:     GPLv3+ (GNU GENERAL PUBLIC LICENSE version 3 or later)

;;; Commentary:

;; Lexer.

;; Due to the verycomplex grammar of Coq, and to the architecture of
;; smie, we deambiguate all kinds of tokens during lexing.  This is a
;; complex piece of code but it allows for all smie goodies.
;; Some examples of deambigations:
;; - We distinguish ":=" from ":= inductive" to avoid the circular precedence
;;   constraint ":= < | < ; < :=" where ":= < |" is due to Inductive
;;   definitions, "| < ;" is due to tactics precedence, "; < :=" is due to
;;   "let x:=3; y:=4 in...".
;; - We distinguish the ".-selector" from the terminator "." for
;;   obvious reasons.
;; - We consider qualified.names as one single token for obvious reasons.
;; - We distinguish the "Module M." from "Module M := exp." since the first
;;   opens a new scope (closed by End) whereas the other doesn't.
;; - We drop "Program" because it's easier to consider "Program Function"
;;   as a single token (which behaves like "Function" w.r.t indentation and
;;   parsing) than to get the parser to handle it correctly.
;; - We identify the different types of bullets (First approximation).
;; - We distinguish "with match" from other "with".

;;; Code:

(require 'coq-indent)
(require 'coq-syntax)                   ;For coq-keywords-save-strict!
(require 'smie)

; debugging
;(defmacro measure-time (&rest body)
;  "Measure the time it takes to evaluate BODY."
;  `(let ((time (current-time)))
;     ,@body
;     (message "%.06f" (float-time (time-since time)))))

(defcustom coq-smie-monadic-tokens '((";;" . ";; monadic")("do" . "let monadic")("<-" . "<- monadic")(";" . "in monadic"));
  "This contains specific indentation token pairs, similar to
`coq-smie-user-tokens' but dedicated to monadic operators. These
tokens have no builtin syntax except the one defined by this
variable so that users can change the syntax at will.

The default value supports ext-lib (x <- e ;; e) and
CompCert (do x <- e ; e) styles.

There are two types of monadic syntax with specific tokens: one
with a starting token (like do):

  \"let monadic\" E \"<- monadic\" E \"in monadic\" E

and the other without:

  E \"<- monadic\" E \";; monadic\" E

Th goal of this variable is to give concrete syntax to these
\"xxx monadic\" tokens."
 :type '(alist :key-type string :value-type string)
  :group 'coq)

(defcustom coq-smie-user-tokens nil
  "Alist of (syntax . token) pairs to extend the coq smie parser.
These are user configurable additional syntax for smie tokens.  It
allows to define alternative syntax for smie token.  Typical
example: if you define a infix operator \"xor\" you may want to
define it as a new syntax for token \"or\" in order to have the
indentation rules of or applied to xor.  Other exemple: if you
want to define a new notation \"ifb\" ... \"then\" \"else\" then
you need to declare \"ifb\" as a new syntax for \"if\" to make
indentation work well.

An example of cofiguration is:

(setq coq-smie-user-tokens '((\"xor\" . \"or\") (\"ifb\" . \"if\")))

to have token \"xor\" and \"ifb\" be considered as having
repectively same priority and associativity as \"or\" and \"if\".

For monadic notations, see `coq-smie-monadic-tokens' instead."
  :type '(alist :key-type string :value-type string)
  :group 'coq)


(defalias 'coq--string-suffix-p
  ;; Replacement for emacs < 24.4, borrowed from sindikat at
  ;; stackoverflow efficient if bytecompiled, builtin version is
  ;; probably better when it exists
  (if (fboundp 'string-suffix-p)
      'string-suffix-p
    (lambda (str1 str2 &optional ignore-case)
  "Return non-nil if STR1 is a prefix of STR2.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((begin2 (- (length str2) (length str1)))
	(end2 (length str2)))
    (when (< begin2 0) (setq begin2 0)) ; to avoid negative begin2
    (eq t (compare-strings str1 nil nil str2 begin2 end2 ignore-case))))))



;; As any user defined notation ending with "." will break
;; proofgeneral synchronization anyway, let us consider that any
;; combination of symbols ending with "." is an end of command for
;; indentation purposes. One noticeable exception is .. that may
;; happen inside notations and is dealt with by pg synchro.
(defun coq-dot-friend-p (s)
  (and (not (string-equal ".." s)) ;; string-equal because ... should return t.
       (string-match "[^[:word:]]\\.\\'" s)))

; for debuging
(defun coq-time-indent ()
  (interactive)
  (let ((deb (float-time)))
    (smie-indent-line)
    (message "time: %S"(- (float-time) deb))))

(defun coq-time-indent-region (beg end)
  (interactive "r")
  (let ((deb (float-time)))
    (indent-region beg end nil)
    (message "time: %S"(- (float-time) deb))))


(defun coq-is-inside-enclosing (bound)
  (save-excursion
    ;; This may fail but we need to see where we stopped
    (coq-smie-search-token-backward '("#dummy#" "{") bound)
    (if (= (point) bound) nil ; we did not cross som paren-like
      (let ((backtok (coq-smie-backward-token)))
        (cond
         ((and (string-equal "" backtok) (eq ?| (char-after))(eq ?{ (char-before))) "{|")
         ((not (string-equal backtok "")) backtok)
         (t (char-to-string (char-before))))))))

;; Fragile: users can define tactics with uppercases... Returns t if
;; we are inside a tactic and not inside a record notation. Ideally we
;; would like to know if the interpretation scope is term or tactic
;; (i.e. detect if we are inside a term inside a tactic) but this is
;; almost impossible sytntactically: how to tell the difference
;; between tactics and tacticals? In "repeat (f x)" and "apply (f x)"
;; f x is a tactic (resp. a terms). Only Coq knows, we could analyse
;; coq grammar tacticals. This is not too problematic here since only
;; in records the indentation changes (maily for ";").
(defun coq-smie-is-tactic ()
  (let* ((pos (point))
         (cmdstrt (save-excursion (coq-find-real-start)))
         (enclosing (coq-is-inside-enclosing cmdstrt)))
    (cond
     ((string-equal enclosing "{|") nil)
     (t (save-excursion
          (goto-char cmdstrt)
          (let ((case-fold-search nil))
            (not (looking-at "[[:upper:]]"))))))))

(defun coq-smie-is-ltacdef ()
  (let ((case-fold-search nil))
    (save-excursion
      (coq-find-real-start)
      (looking-at "\\(\\(Local\\|Global\\)\\s-+\\)?\\(Ltac\\|Tactic\\s-+Notation\\)\\s-"))))

(defun coq-smie-is-inside-parenthesized-tactic ()
  (and (coq-smie-is-tactic) ;; fragile (uppercase test only)
       (save-excursion
	 (let ((pt (point))
	       ;; we need to go one character forwardto avoid the
	       ;; coq-smie-search-token-backward below to fail on "{"
	       (strt (and (coq-script-parse-cmdend-backward)
			  (+ 1 (point)))))
	   (goto-char pt)
	   ;; looking for a dummy token to see if we fail before reaching
	   ;; strt, which means that we were in a prenthesized expression.
	   (coq-smie-search-token-backward '("#dummy#") strt)
	   (> (point) strt)))))

(defun coq-smie-.-deambiguate ()
  "Return the token of the command terminator of the current command.
For example in:

Proof.       or        Proof with ... .

the token of the \".\" is \". proofstart\".

But in

intros.      or        Proof foo.

the token of \".\" is simply \".\"."
  (save-excursion
    (let ((p (point)))
      (coq-find-real-start) ; Move to real start of command.
      (cond
       ((looking-at "BeginSubproof\\>") ". proofstart")
       ((looking-at "Proof\\>")
	(forward-char 5)
        (forward-comment (point-max))
	(if (looking-at "\\.\\|with\\|using") ". proofstart" "."))
       ((or (looking-at "Next\\s-+Obligation\\>")
	    (coq-smie-detect-goal-command))
	(save-excursion
	  (goto-char (+ p 1))
          (let ((tok (smie-default-forward-token)))
            (cond
             ;; If the next token is "Proof", then the current command does
             ;; introduce a proof, but the user opted to use the explicit
             ;; "Proof" command, so the current command doesn't itself start
             ;; the proof.
             ((equal tok "Proof") ".")
             ;; If the next command is a new definition, then the current
             ;; command didn't actually start a proof!
             ((member tok '("Let" "Definition" "Inductive" "Fixpoint")) ".")
             (t ". proofstart")))))
       ((equal (coq-smie-module-deambiguate) "Module start")
	". modulestart")
       (t ".")))))


(defun coq-smie-complete-qualid-backward ()
  "Return the qualid finishing at the current point."
  (let ((p (point)))
    (re-search-backward "[^.[:alnum:]_@]")
    (forward-char 1)
    (buffer-substring (point) p)))


(defun coq-smie-find-unclosed-match-backward ()
  (let ((tok (coq-smie-search-token-backward '("with" "match" "lazymatch" "multimatch" "."))))
    (cond
     ((null tok) nil)
     ((equal tok ".") nil)
     ((equal tok "with")
      (coq-smie-find-unclosed-match-backward)
      (coq-smie-find-unclosed-match-backward))
     (t t) ;; all variants of match
     )))

;; point supposed to be at start of the "with"
(defun coq-smie-with-deambiguate()
  (let ((p (point)))
    (if (coq-smie-find-unclosed-match-backward)
	"with match"
      (goto-char p)
      (coq-find-real-start)
      (cond
       ((looking-at "\\(Co\\)?Inductive") "with inductive")
       ((looking-at "\\(Co\\)?Fixpoint\\|Function\\|Program\\|Lemma") "with fixpoint")
       ((looking-at "Module\\|Declare") "with module")
       (t "with")))))



;; A variant of smie-default-backward-token that recognize "." and ";"
;; as single token even if glued at the end of another symbols.

(defun coq-backward-token-fast-nogluing-dot-friends ()
  (forward-comment (- (point)))
  (let* ((pt (point))
         (tok-punc (skip-syntax-backward "."))
         (str-punc (buffer-substring-no-properties pt (point))))
    (if (zerop tok-punc) (skip-syntax-backward "w_'"))
    ;; Special case: if the symbols found end by "." or ";",
    ;; then consider this last letter alone as a token
    (when (and (not (zerop tok-punc)) (string-match "\\s.+[.;]" str-punc))
      (skip-syntax-forward ".")
      (forward-char -1))
    (buffer-substring-no-properties pt (point))))

(defun coq-forward-token-fast-nogluing-dot-friends ()
  (forward-comment (point-max))
  (let* ((pt (point))
         (tok-punc (skip-syntax-forward "."))
         (str-punc (buffer-substring-no-properties pt (point))))
    (if (zerop tok-punc) (skip-syntax-forward "w_'"))
    ;; Special case: if the symbols found end by "." or ";",
    ;; then consider this last letter alone as a token
    (when (and (not (zerop tok-punc)) (string-match "\\s.+[.;]" str-punc))
      (forward-char -1))
    (buffer-substring-no-properties pt (point))))

;; ignore-between is a description of pseudo delimiters of blocks that
;; should be jumped when searching. There is a bad interaction when
;; tokens and ignore-bteween are not disjoint
(defun coq-smie-search-token-forward (tokens &optional end ignore-between)
  "Search for one of TOKENS between point and END.
If some enclosing parenthesis is reached, stop there and return nil.
Token \".\" is considered only if followed by a space.  Optional
IGNORE-BETWEEN defines opener/closer to ignore during search.
Careful: the search for a opener stays inside the current command (and
inside parenthesis)."
  (unless end (setq end (point-max)))
  (condition-case nil
      (catch 'found
	(while (< (point) end)
	  ;; The default lexer is faster and is good enough for our needs.
	  (let* ((next (coq-forward-token-fast-nogluing-dot-friends))
		 (parop (assoc next ignore-between)))
	    ; if we find something to ignore, we directly jump to the
	    ; corresponding closer
	    (if parop
		(let ((parops ; corresponding matcher may be a list
		       (if (listp parop) (cdr parop)
			 (cons (cdr parop) nil)))) ; go to corresponding closer
		  (when (member
			 (coq-smie-search-token-forward
			  (append parops (cons "." nil))
			  end ignore-between)
			 (cons "." nil))
		  (goto-char (point))
		  next))
	      ;; Do not consider "." when not followed by a space
	      (when (or (not (equal next ".")) ; see backward version
			(looking-at "[[:space:]]"))
		(cond
		 ((and (zerop (length next))
		       (or (equal (point) (point-max)) ; protecting char-after next line
			   (equal (char-syntax ?\)) (char-syntax (char-after)))))
		  (throw 'found nil))
		 ((zerop (length next)) ;; capture other characters than closing parent
		  ;; don't use smmie-forward-sexp here
		  (let ((forward-sexp-function nil)) (forward-sexp 1)))
		 ((member next tokens) (throw 'found next))))))))
    (scan-error nil)))


;; ignore-between is a description of pseudo delimiters of blocks that
;; should be jumped when searching. There is a bad interaction when
;; tokens and ignore-bteween are not disjoint
(defun coq-smie-search-token-backward (tokens &optional end ignore-between)
  "Search for one of TOKENS between point and END.
If some enclosing parenthesis is reached, stop there and return nil.
Token \".\" is considered only if followed by a space.
optional IGNORE-BETWEEN defines opener/closer to ignore during
search.
Careful: the search for a opener stays inside the current command (and
inside parenthesis)."
  (unless end (setq end (point-min)))
    (condition-case nil
	(catch 'found
	  (while (> (point) end)
	    ;; The default lexer is faster and is good enough for our needs.
	    (let* ((next (coq-backward-token-fast-nogluing-dot-friends))
		   (parop (rassoc next ignore-between)))
	      ; if we find something to ignore, we directly jump to the
	      ; corresponding openner
	      (if parop
		  (let ((parops ; corresponding matcher may be a list
			 (if (listp (car parop)) (car parop) (cons (car parop) nil))))
		    ; go to corresponding closer or meet "."
		    (when (member
			   (coq-smie-search-token-backward
			    (append parops (cons "." nil))
			    end ignore-between)
			   (cons "." nil))
		      next))
		;; Do not consider "." when not followed by a space
		;(message "SPACE?: %S , %S , %S" next next (looking-at ".[[:space:]]"))
		(when (or (not (equal next "."))
			  (looking-at "\\.[[:space:]]"))
		  (cond
		   ((and (zerop (length next))
			 (or (equal (point) (point-min)) ; protecting char-before next line
			     (equal (char-syntax ?\() (char-syntax (char-before)))))
		    (throw 'found nil))
		   ((zerop (length next))
		    ;; don't use smmie-forward-sexp here
		    (let ((forward-sexp-function nil)) (forward-sexp -1)))
		   ((member next tokens) (throw 'found next))))))))
      (scan-error nil)))

(defun coq-lonely-:=-in-this-command ()
  "Return t if there is a lonely \":=\" from (point) to end of command.
Non lonely \":=\" are those corresponding to \"let\" or
\"with\" (module declaration) or those inside parenthesis. this
function is used to detect whether a command is a definition or a
proof-mode starter in Coq."
  (equal (coq-smie-search-token-forward
	  '("." ":=") nil
	  '(("with" . (":=" "signature")) ("let" . "in")))
	 "."))

;; Heuristic to detect a goal opening command: there must be a lonely
;; ":=" until command end.
;; \\|\\(Declare\\s-+\\)?Instance is not detected as it is not
;; syntactically decidable to know if some goals are created. Same for
;; Program Fixpoint but with Program Next Obligation is mandatory for
;; each goal anyway.
(defun coq-smie-detect-goal-command ()
  "Return t if the next command is a goal starting to be indented.
The point should be at the beginning of the command name.
As false positive are more annoying than false negative, return t only
if it is FOR SURE a goal opener.  Put a \"Proof.\" when you want to
force indentation."
  (save-excursion         ; FIXME add other commands that potentialy open goals
    (let ((case-fold-search nil))
      (when (looking-at "\\(Local\\|Global\\)?\
\\(Definition\\|Lemma\\|Theorem\\|Fact\\|Let\\|Class\
\\|Proposition\\|Remark\\|Corollary\\|Goal\
\\|Add\\(\\s-+Parametric\\)?\\s-+Morphism\
\\|Fixpoint\\)\\>") ;; Yes Fixpoint can start a proof like Definition
	(coq-lonely-:=-in-this-command)))))


;; Heuristic to detect a goal opening command: there must be a lonely ":="
(defun coq-smie-module-deambiguate ()
  "Return t if the next command is a goal starting command.
The point should be at the beginning of the command name."
  (save-excursion              ; FIXME Is there other module starting commands?
    (let ((case-fold-search nil))
      (cond
       ((looking-back "with\\s-+" nil) "module nodecl") ; Module that is not a declaration keyword (like in with Module)
       ((looking-at "\\(Module\\|Section\\)\\>")
        (if (coq-lonely-:=-in-this-command) "Module start" "Module def"))))))


;(defun coq-smie-detect-module-or-section-start-command ()
;  "Return t if the next command is a goal starting command.
;The point should be at the beginning of the command name."
;  (save-excursion ; FIXME Is there other module starting commands?
;    (when (and (looking-back "with")
;	       (proof-looking-at "\\(\\(?:Declare\\s-+\\)?Module\\|Section\\)\\>"))
;      (coq-lonely-:=-in-this-command))))


(defconst coq-smie-proof-end-tokens
  ;; '("Qed" "Save" "Defined" "Admitted" "Abort")
  (cons "EndSubproof" (remove "End" coq-keywords-save-strict)))


(defun coq-is-at-command-real-start()
  (equal (point)
	 (save-excursion (coq-find-real-start))))

(defun coq-is-bullet-token (tok) (coq--string-suffix-p "bullet" tok))
(defun coq-is-subproof-token (tok) (coq--string-suffix-p "subproof" tok))
(defun coq-is-dot-token (tok) (or (coq--string-suffix-p "proofstart" tok)
			       (string-equal "." tok)))
(defun coq-is-cmdend-token (tok)
  (or (coq-is-bullet-token tok) (coq-is-subproof-token tok) (coq-is-dot-token tok)))

(defun coq-smie-forward-token ()
  (let ((tok (coq-smie-forward-token-aux)))
    (cond
     ((assoc tok coq-smie-user-tokens)
      (let ((res (assoc tok coq-smie-user-tokens)))
        (cdr res)))
     ((assoc tok coq-smie-monadic-tokens)
      (let ((res (assoc tok coq-smie-monadic-tokens)))
        (cdr res)))
     (tok))))

(defun coq-smie-forward-token-aux ()
  (let ((tok (smie-default-forward-token)))
    (cond
     ;; @ may be  ahead of an id, it is part of the id.
     ((and (equal tok "@") (looking-at "[[:alpha:]_]"))
      (let ((newtok (coq-smie-forward-token))) ;; recursive call
	(concat tok newtok)))
     ;; detecting if some qualification (dot notation) follows that id and
     ;; extend it if yes. Does not capture other alphanumerical token (captured
     ;; below)
     ((and (string-match "@?[[:alpha:]_][[:word:]]*" tok)
	   (looking-at "\\.[[:alpha:]_]")
	   (progn (forward-char 1)
		  (let ((newtok (coq-smie-forward-token))) ; recursive call
		    (concat tok "." newtok)))))
     ((member tok '("." "..."))
      ;; swallow if qualid, call backward-token otherwise
      (cond
       ((member (char-after) '(?w ?_))  ;(looking-at "[[:alpha:]_]") ;; extend qualifier
	(let ((newtok (coq-smie-forward-token))) ;; recursive call
	  (concat tok newtok)))
       (t (save-excursion (coq-smie-backward-token))))) ;; recursive call
     ((or (string-match coq-bullet-regexp-nospace tok)
	  (member tok '("=>" ":=" "::=" "exists" "in" "as" "∀" "∃" "→" "∨" "∧" ";"
			"," ":" "eval")))
      ;; The important lexer for indentation's performance is the backward
      ;; lexer, so for the forward lexer we delegate to the backward one when
      ;; we can.
      (save-excursion (coq-smie-backward-token)))

     ;; easier to return directly than calling coq-smie-backward-token
     ((member tok '("lazymatch" "multimatch")) "match")

     ;; detect "with signature", otherwies use coq-smie-backward-token
     ((equal tok "with")
      (let ((p (point)))
	(if (equal (smie-default-forward-token) "signature")
	    "with signature"
	  (goto-char p)
	  (save-excursion (coq-smie-backward-token)))))

     ((member tok '("transitivity" "symmetry" "reflexivity"))
      (let ((p (point)))
	(if (and (equal (smie-default-forward-token) "proved")
		 (equal (smie-default-forward-token) "by"))
	    "xxx provedby"
	  (goto-char p)
	  tok))) ; by tactical

     ((member tok '("Module")) ; TODO: Declare
      (let ((pos (point))
	    (next (smie-default-forward-token)))
	(unless (equal next "Type") (goto-char pos))
	(save-excursion (coq-smie-backward-token))))

     ((member tok '("End"))
      (save-excursion (coq-smie-backward-token)))

     ((member tok '("do"))
      (save-excursion (coq-smie-backward-token-aux)))

     ; empty token if a prenthesis is met.
     ((and (zerop (length tok)) (looking-at "{|")) (goto-char (match-end 0)) "{|")

     ;; this must be after detecting "{|":
     ((and (zerop (length tok)) (eq (char-after) ?\{))
      (if (equal (save-excursion (forward-char 1) (coq-smie-backward-token))
		 "{ subproof")
	  (progn (forward-char 1) "{ subproof")
	tok))

     ((and (zerop (length tok)) (eq (char-after) ?\}))
      (if (equal (save-excursion (forward-char 1)
				 (coq-smie-backward-token))
		 "} subproof")
	  (progn (forward-char 1) "} subproof")
	tok))
     ((and (equal tok "|") (eq (char-after) ?\}))
      (goto-char (1+ (point))) "|}")
     ((member tok coq-smie-proof-end-tokens) "Proof End")
     ((member tok '("Obligation")) "Proof")
     ;; FIXME: this case should be useless now that we replace
     ;; smie-default-forward... by a smarter function.
     ((coq-dot-friend-p tok) ".")
     ;; Try to rely on backward-token for non empty tokens: bugs (hangs)
     ;; ((not (zerop (length tok))) (save-excursion (coq-smie-backward-token)))
     ;; return it.
     (tok)
     )))



;; ":= with module" is really to declare some sub-information ":=
;; with" is for mutual definitions where both sides are of the same
;; level
(defun coq-smie-:=-deambiguate ()
  (let* ((orig (point))
         (cmdstrt (save-excursion (coq-find-real-start)))
         (corresp (coq-smie-search-token-backward
		   '("let" "Inductive" "CoInductive" "{|" "." "with" "Module" "where"
                     "Equations")
		   cmdstrt '((("let" "with") . ":=")))))
    (cond
     ((member corresp '("Equations")) ":= equations")
     ((equal corresp "with")
      (let ((corresptok (coq-smie-with-deambiguate)))
	(cond ;; recursive call if the with found is actually et with match
	 ((equal corresptok "with match") (coq-smie-:=-deambiguate))
	 ((equal corresptok "with inductive") ":= inductive")
	 ((equal corresptok "with module") ":= with module")
	 (t ":=")
	 )))
     ((equal corresp "Module")
      (let ((p (point)))
	(if (equal (smie-default-backward-token) "with")
	    ":= with module"
	  (goto-char p)
	  ":= module")))
     ((member corresp '("Inductive" "CoInductive" "Variant")) ":= inductive")
     ((equal corresp "let") ":= let")
     ((equal corresp "where") ":= inductive") ;; inductive or fixpoint, nevermind
     ((or (eq ?\{ (char-before))) ":= record")
     ((equal (point) cmdstrt)
      (if (looking-at "Equations") ":="
        ":= def")) ; := outside of any parenthesis
     (t ":=")
     ))) ; a parenthesis stopped the search


(defun coq-smie-semicolon-deambiguate ()
  (let* ((pos (point))
         (cmdstrt (save-excursion (coq-find-real-start)))
         (istac (or (coq-smie-is-tactic)
                    (coq-smie-is-ltacdef)
                    (coq-smie-is-inside-parenthesized-tactic)))
         (enclosing (coq-is-inside-enclosing cmdstrt)))
    (cond
     (istac "; tactic")
     ;; looking for a dummy token to see if we fail before reaching
     ;; strt, which means that we were in a prenthesized expression.
     ((string-equal enclosing "{ subproof")  "; tactic")
     ((member enclosing '("{" "{|"))  "; record")
     (t
      (if (save-excursion (goto-char cmdstrt) (looking-at "Equations"))
          "; equations"
        ";")))))

(defun coq-smie-backward-token ()
  (let ((tok (coq-smie-backward-token-aux)))
    (cond
     ((assoc tok coq-smie-user-tokens)
      (let ((res (assoc tok coq-smie-user-tokens)))
        (cdr res)))
     ((assoc tok coq-smie-monadic-tokens)
      (let ((res (assoc tok coq-smie-monadic-tokens)))
        (cdr res)))
     (tok))))

(defun coq-smie-backward-token-aux ()
  (let* ((tok (smie-default-backward-token)))
    (cond
     ;; Distinguish between "," from quantification and other uses of
     ;; "," (tuples, tactic arguments)
     ((equal tok ",")
      (save-excursion
	(let ((backtok (coq-smie-search-token-backward
			'("forall" "∀" "∃" "exists" "|" "match" "lazymatch" "multimatch" "."))))
	  (cond
	   ((member backtok '("forall" "∀" "∃")) ", quantif")
	   ((equal backtok "exists") ; there is a tactic called exists
	    (if (equal (coq-smie-forward-token) ;; recursive call
		       "quantif exists")
		", quantif" tok))
	   (t tok)))))

     ; Same for ";" : record field separator, tactic combinator, etc
     ((equal tok ";")
      (save-excursion (coq-smie-semicolon-deambiguate)))

     ;; trying to discriminate between bollean operator || and tactical ||.
     ((equal tok "||")
      (save-excursion
	(let ((backtok (coq-smie-search-token-backward '("." ";" "Ltac" "(" "[" "{"))))
	  (cond
	   ((member backtok '("." "Ltac")) "|| tactic")
	   ((and (equal backtok ";")
		 (or (forward-char) t)
		 (equal (coq-smie-backward-token) "; tactic")) ;; recursive
	    "|| tactic")
	   ;; this is wrong half of the time but should not harm indentation
	   ((and (equal backtok nil) (eq (char-before) '?\()) "||")
	   ((equal backtok nil)
	    (if (or (eq (char-before) '?\[)
		    (and (eq (char-before) '?\{)
			 (equal (coq-smie-backward-token) "{ subproof"))) ;; recursive call
		"|| tactic"
	      "||"))))))

     ;; This may be part of monadic notation, so detect other uses of "do".
     ((equal tok "do")
      (save-excursion
        (forward-char 2)
        (smie-default-forward-token)
        (smie-default-backward-token)
        (if (looking-at "[0-9]") "do ltac"
          "do")))


     ; Same for "->" : rewrite or intro arg or term's implication
     ; FIXME: user defined arrows will be considered a term
     ((equal tok "->")
      (save-excursion
	(let ((backtok (coq-smie-search-token-backward '("intro" "intros" "rewrite" "."))))
	  (cond
	   ((equal backtok ".") "->")
	   ((equal backtok nil) "->")
	   (t "-> tactic")))))

     ;; "<-" is a commonly used token for monadic notations, we should
     ;; discrimnate between "rewrite ... <-" and other uses of "<-".
     ((equal tok "<-")
      (save-excursion
	(let ((backtok (coq-smie-search-token-backward '("intro" "intros" "rewrite" "."))))
	  (cond
	   ((equal backtok ".") "<-")
	   ((equal backtok nil) "<-")
	   (t "<- tactic")))))


     ((equal tok "Module")
      (save-excursion
	;(coq-find-real-start)
	(coq-smie-module-deambiguate)))

     ((member tok '("lazymatch" "multimatch")) "match")

     ((equal tok "tryif") "if")

     ;; rhaaa... Some peolple use "End" as a id...
     ((equal tok "End")
      (if (coq-is-at-command-real-start) "end module" tok))

     ;; FIXME: this is a parenthesis
     ((and (equal tok "|") (eq (char-before) ?\{))
      (goto-char (1- (point))) "{|")

     ;; curly braces can be beginproof/endproof or record stuff.
     ((and (zerop (length tok)) (member (char-before) '(?\{ ?\}))
           (save-excursion
             (forward-char -1)
             (if (and (looking-at "{")
                      (looking-back "\\(\\[?\\w+\\]?\\s-*:\\s-*\\)" nil t))
                 (goto-char (match-beginning 0)))
             (let ((nxttok (coq-smie-backward-token))) ;; recursive call
               (coq-is-cmdend-token nxttok))))
      (forward-char -1)
      (if (looking-at "}") "} subproof"
        (if (and (looking-at "{")
                 (looking-back "\\(\\[?\\w+\\]?\\s-*:\\s-*\\)" nil t))
            (goto-char (match-beginning 0)))
        "{ subproof"
        ))

     ;; ((and (zerop (length tok)) (member (char-before) '(?\{ ?\}))
     ;;       (save-excursion
     ;;         (forward-char -1)
     ;;         (let ((nxttok (coq-smie-backward-token))) ;; recursive call
     ;;           (coq-is-cmdend-token nxttok))))
     ;;  (forward-char -1)
     ;;  (if (looking-at "{") "{ subproof" "} subproof"))

     ((and (equal tok ":") (looking-back "\\<\\(constr\\|ltac\\|uconstr\\)"
                                         (- (point) 7)))
      ": ltacconstr")

     ((member tok '(":=" "::="))
      (save-excursion
	(save-excursion (coq-smie-:=-deambiguate))))

     ((equal tok "=>")
      (save-excursion
	(let ((corresp (coq-smie-search-token-backward
			'("|" "match" "lazymatch" "multimatch" "fun" ".")
			nil '((("match" "lazymatch" "multimatch") . "end") ("fun" . "=>")))))
	  (cond
	   ((member corresp '("fun")) "=> fun") ; fun
	   (t tok)))))

     ;; FIXME: no token should end with "." except "." itself
     ; for "unfold in *|-*."
     ((member tok '("*." "-*." "|-*." "*|-*."))
      (forward-char (- (length tok) 1))
      (coq-smie-.-deambiguate))
     ; for "unfold in *|-*;"
     ((member tok '("*;" "-*;" "|-*;" "*|-*;"))
      ;; FIXME; can be "; ltac" too
      (forward-char (- (length tok) 1)) "; tactic")
     ;; bullet detected, is it really a bullet? we have to traverse
     ;; recursively any other bullet or "n:{" "}". this is the work of
     ;; coq-empty-command-p
     ((and (string-match coq-bullet-regexp-nospace tok)
	   (save-excursion (coq-empty-command-p)))
      (concat tok " bullet"))

     ((and (member tok '("exists" "∃"))
	   (save-excursion
	     ;; recursive call looking at the ptoken immediately before
	     (let ((prevtok (coq-smie-backward-token)))
	       ;; => may be wrong here but rare (have "=> ltac"?)
	       (not (or (coq-is-cmdend-token prevtok)
			(member prevtok '("; tactic" "[" "]" "|" "=>")))))))
      "quantif exists")

     ((equal tok "∀") "forall")
     ((equal tok "→") "->")
     ((equal tok "∨") "\\/")
     ((equal tok "∧") "/\\")

     ((equal tok "with") ; "with" is a nightmare: at least 4 different uses
      (save-excursion (coq-smie-with-deambiguate)))
     ((equal tok "where")
      "where")

     ((and (equal tok "signature")
	   (equal (smie-default-backward-token) "with"))
      "with signature")

     ((equal tok "by")
      (let ((p (point)))
	(if (and (equal (smie-default-backward-token) "proved")
		 (member (smie-default-backward-token)
			 '("transitivity" "symmetry" "reflexivity")))
	    "xxx provedby"
	  (goto-char p)
	  tok))) ; by tactical

     ((equal tok "as")
      (save-excursion
	(let ((prev-interesting
	       (coq-smie-search-token-backward
		'("match" "lazymatch" "multimatch" "Morphism" "Relation" "." ". proofstart"
		  "{ subproof" "} subproof" "as")
		nil
		'((("match" "lazymatch" "multimatch" "let") . "with") ("with" . "signature")))))
	  (cond
	   ((member prev-interesting '("match" "lazymatch" "multimatch")) "as match")
	   ((member prev-interesting '("Morphism" "Relation")) "as morphism")
	   (t tok)))))

     ((equal tok "by")
      (let ((p (point)))
	(if (and (equal (smie-default-backward-token) "proved")
		 (member (smie-default-backward-token)
			 '("transitivity" "symmetry" "reflexivity")))
	    "xxx provedby"
	  (goto-char p)
	  tok))) ; by tactical


     ((equal tok "eval")
      (if (member (save-excursion
		    (forward-char 4)
		    (smie-default-forward-token))
		  '("red" "hnf" "compute" "simpl" "cbv" "lazy" "unfold" "fold" "pattern"))
	  "eval in" tok))
     

     ((equal tok "in")
      (save-excursion
	(let ((prev-interesting
	       (coq-smie-search-token-backward
		'("let" "match" "lazymatch" "multimatch" ;"eval" should be "eval in" but this is not supported by search-token-backward
		  "." ) nil
		'((("match" "lazymatch" "multimatch") . "with") (("let" ;"eval"
				       ) . "in")))))
	  (cond
	   ((member prev-interesting '("." nil)) "in tactic")
	   ((equal prev-interesting "let") "in let")
	   ;((equal prev-interesting "eval in") "in eval"); not detectable by coq-smie-search-token-backward
	   ((equal prev-interesting "match") "in match")
	   (t "in tactic")))))

     ((and (eq (char-before) ?@) (member (char-syntax (char-after)) '(?w ?_)))
      (forward-char -1)
      (concat "@" tok))

     ((member tok coq-smie-proof-end-tokens) "Proof End")

     ((member tok '("." "..."))
      ;; Distinguish field-selector "." from terminator "." from module
      ;; qualifier.
      (let ((nxtnxt (char-after (+ (point) (length tok)))))
	(if (eq nxtnxt ?\() ". selector"
          (if (eq nxtnxt ?}) ;; dot immediately followed by closesubproof. 
              "."
	    (if (or (null nxtnxt) (eq (char-syntax nxtnxt) ?\ ))
	        ;; command terminator: ". proofstart" et al
	        (save-excursion (forward-char (- (length tok) 1))
			        (coq-smie-.-deambiguate))
	      (cond
               ((eq (char-syntax nxtnxt) ?w)
	        (let ((newtok (coq-smie-complete-qualid-backward)))
		  ;; qualified name
		  (concat newtok tok)))
	       (t ". selector")))))))  ;; probably a user defined syntax

     ((and (and (eq (char-before) ?.) (member (char-syntax (char-after))
					      '(?w ?_))))
      (forward-char -1)
      (let ((newtok (coq-smie-backward-token))) ; recursive call
	(concat newtok "." tok)))

     ((coq-dot-friend-p tok) ".")
     (tok))))


(defcustom coq-indent-box-style nil
  "If non-nil, Coq mode will try to indent with a box style (SMIE code only).
Box style looks like this:

Lemma foo: forall n,
             n = n.

instead of:

Lemma foo: forall n,
  n = n."
  :type 'boolean
  :group 'coq)

(defun coq-indent-safep (indent)
  (>= indent 0))

(defcustom coq-indent-proofstart 2
  "Number of spaces used to indent after a proof start."
  :type 'integer
  :group 'coq
  :safe #'coq-indent-safep)

(defcustom coq-indent-semicolon-tactical 2
  "Number of spaces used to indent after the 1st tactical semicolon of a serie.
If set to 0, indentation is as follows:
tac1;
tac2;
tac3;
tac4.

If set to 2 (default):
tac1;
  tac2;
  tac3;
  tac4."
  :type 'integer
  :group 'coq
  :safe #'coq-indent-safep)

(defcustom coq-indent-modulestart 2
  "Number of spaces used to indent after a module or section start."
  :type 'integer
  :group 'coq
  :safe #'coq-indent-safep)

(defcustom coq-smie-after-bolp-indentation 2
  "Number of spaces used to indent after a quantifier *not* on its own line.

the number of space is meant \"from the column on which the quantifier
would be if it were on its own line\".
for example, if set to 0 the indentation is as follows:

   Lemma foo: forall x:nat,
     x <= 0 -> x = 0.

If it is set to 2 (default) it is as follows:

   Lemma foo: forall x:nat,
       x <= 0 -> x = 0."
  :type 'integer
  :group 'coq
  :safe #'coq-indent-safep)

(defcustom coq-match-indent 2
  "Number of space used to indent cases of a match expression.
If the \"|\" separator is used, indentation will be reduced by 2.
For example the default value 2 makes indetation like this:

match n with
  O => ...
| S n => ...
end

Typical values are 2 or 4."
  :type 'integer
  :group 'coq
  :safe #'coq-indent-safep)

;; - TODO: remove tokens "{ subproof" and "} subproof" but they are
;;         needed by the lexers at a lot of places.
;; - FIXME: This does not know about Notations.
;; - TODO Actually there are two grammars: one at script level, for
;;   indenting each command with respect to the previous commands, and
;;   a standard one inside commands. Separating the two grammars would
;;   greatly simplify this file. We should ask Stefan Monnier how to
;;   have two grammars with smie.
(defconst coq-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((exp
       (exp ":= equations" exp) (exp ":= def" exp)
       (exp ":=" exp) (exp ":= inductive" exp)
       (exp "||" exp) (exp "|" exp) (exp "=>" exp) (exp "; equations" exp)
       (exp "xxx provedby" exp) (exp "as morphism" exp)
       (exp "with signature" exp)
       ("match" matchexp "with match" exp "end") ;expssss
       ("let" assigns "in let" exp)
       ("let monadic" assigns "in monadic" exp)
       ;;("eval in" assigns "in eval" exp) disabled
       ("fun" exp "=> fun" exp) ("if" exp "then" exp "else" exp)
       ("quantif exists" exp ", quantif" exp)
       ("forall" exp ", quantif" exp)
;;;
       (exp "<- monadic" exp) (exp ";; monadic" exp)
       ("(" exp ")") ("{|" exps "|}") ("{" exps "}")
       (exp "; tactic" exp) (exp "in tactic" exp) (exp "as" exp)
       (exp "by" exp) (exp "with" exp) (exp "|-" exp)
       (exp ":" exp) (exp ":<" exp) (exp "," exp)
       (exp "->" exp) (exp "<->" exp) (exp "&" exp)
       (exp "/\\" exp) (exp "\\/" exp)
       (exp "==" exp) (exp "=" exp) (exp "<>" exp) (exp "<=" exp)
       (exp "<" exp) (exp ">=" exp) (exp ">" exp)
       (exp "=?" exp) (exp "<=?" exp) (exp "<?" exp)
       (exp "+" exp) (exp "-" exp)
       (exp "*" exp) (exp "&&" exp)
       (exp "^" exp)
       (exp ": ltacconstr" exp)(exp ". selector" exp))
      ;; Having "return" here rather than as a separate rule in `exp' causes
      ;; it to be indented at a different level than "with".
      (matchexp (exp) (exp "as match" exp) (exp "in match" exp)
		(exp "return" exp) )
      (exps (affectrec) (exps "; record" exps))
      (affectrec (exp ":= record" exp))
      (assigns  (exp ":= let" exp) (exp "<- monadic" exp))
      ;;(assigns "; record" assigns)

      (moduledef (moduledecl ":= module" exp))
      (moduledecl (exp) (exp ":" moduleconstraint)
		  (exp "<:" moduleconstraint))
      (moduleconstraint
       (exp) (exp ":= with module" exp)
       (moduleconstraint "with module" "module nodecl" moduleconstraint))

      ;; To deal with indentation inside module declaration and inside
      ;; proofs, we rely on the lexer. The lexer detects "." terminator of
      ;; goal starter and returns the ". proofstart" and ". moduelstart"
      ;; tokens.
      (bloc ("{ subproof" commands "} subproof")
	    (". proofstart" commands  "Proof End")
	    (". modulestart" commands  "end module" exp)
	    (moduledecl) (moduledef)
	    (exp))

      (commands (commands "." commands)
		(commands "- bullet" commands)
		(commands "+ bullet" commands)
		(commands "* bullet" commands)
		(commands "-- bullet" commands)
		(commands "++ bullet" commands)
		(commands "** bullet" commands)
		(commands "--- bullet" commands)
		(commands "+++ bullet" commands)
		(commands "*** bullet" commands)
		(commands "---- bullet" commands)
		(commands "++++ bullet" commands)
		(commands "**** bullet" commands)
		;; "with" of mutual definition should act like "."
		;; same for "where" (introduction of a notation
		;; after a inductive or fixpoint)
		(commands "with inductive" commands)
		(commands "with fixpoint" commands)
		(commands "where" commands)
		(bloc)))


    ;; Resolve the "trailing expression ambiguity" as in "else x -> b".
    ;; each line orders tokens by increasing priority
    ;; | C x => fun a => b | C2 x => ...
    ;;'((assoc "=>") (assoc "|")  (assoc "|-" "=> fun")) ; (assoc ", quantif")
    '((assoc "- bullet") (assoc "+ bullet") (assoc "* bullet")
      (assoc "-- bullet") (assoc "++ bullet") (assoc "** bullet")
      (assoc "--- bullet") (assoc "+++ bullet") (assoc "*** bullet")
      (assoc "---- bullet") (assoc "++++ bullet") (assoc "**** bullet")
      (assoc ".")
      (assoc "with inductive" "with fixpoint" "where"))
    '((assoc ":= equations") (assoc ":= def" ":= inductive")
      (assoc "|") (assoc "; equations") (assoc "=>") (assoc ":=")
      (assoc "xxx provedby")
      (assoc "as morphism") (assoc "with signature") (assoc "with match")
      (assoc "in let" "in monadic")
      (assoc "in eval") (assoc "=> fun") (assoc ", quantif") (assoc "then")
      (assoc "|| tactic") ;; FIXME: detecting "+ tactic" and "|| tactic" seems impossible
      (assoc "; tactic") (assoc "in tactic") (assoc "as" "by") (assoc "with")
      (assoc "|-") (assoc ":" ":<") (assoc ",")
      (assoc "else")
      (assoc "->") (assoc "<->")
      (assoc "\\/") (assoc "&") (assoc "/\\")
      (assoc "==") (assoc "=") (assoc "<" ">" "<=" ">=" "<>")
      (assoc "=?") (assoc "<=?") (assoc "<?")
      (assoc ";; monadic") (assoc "<- monadic")
      (assoc "^")
      (assoc "||") ;; FIXME: detecting "+ tactic" and "|| tactic" seems impossible
      (assoc "+") (assoc "-") (assoc "*")(assoc "&&")
      (assoc ": ltacconstr") (assoc ". selector"))
    '((assoc ":" ":<")  (assoc "<"))
    '((assoc ". modulestart" "." ". proofstart") (assoc "Module def")
      (assoc "with module" "module nodecl") (assoc ":= module")
      (assoc ":= with module")  (assoc ":" ":<"))
    '((assoc ":= def") (assoc "; record") (assoc ":= record"))))
  "Parsing table for Coq.  See `smie-grammar'.")
;; FIXME:
; Record rec:Set :=     {
;                   fld1:nat;
;                   fld2:nat;
;                   fld3:bool
;                 }.
; FIXME: as is sometimes a "as morphism" but not detected as such
;; Add Parametric Morphism (A : Type) : (mu (A:=A))
;;     with signature Oeq ==> Oeq
;;                    as mu_eq_morphism.

;; FIXME: have a different token for := corresponding to a "fix" (not
;; Fixpoint)
;;Definition join l : key -> elt -> t -> t :=
;;      match l with
;;        | Leaf => add
;;        | Node ll lx ld lr lh => fun x d =>
;;                                   fix join_aux (r:t) : t
;;        := match r with   <---- ??
;;             | Leaf =>  add x d l


;; Returns the column of the beginning of current atomic tactic (non
;; composed). Returns the command start column if not found.
(defun coq-find-with-related-backward()
  (let ((cmd-start (save-excursion (coq-find-real-start))))
    (save-excursion
      ;; no point in going further the start of the command
      ;; let us find a tactical between it and point
      (let ((tok (coq-smie-search-token-backward '(";" "||" "|" "+") cmd-start)))
        ;; hopefully we found the start of the current (non composed)tactic
        ;; move point after the token found (if not found it will not move)
        (forward-char (length tok))
        (forward-comment (point-max)); skip spaces
        ;; (coq-find-not-in-comment-forward "[^;+[:space:]|]")
        (current-column)))))

(defun coq-is-at-first-line-of-def-decl ()
  (let ((pt (point)))
    (save-excursion
      (and
       (member (coq-smie-backward-token) '(":" ":="))
       (equal (line-number-at-pos) (line-number-at-pos pt))
       (or (back-to-indentation) t)
       (looking-at "Lemma\\|Defintion\\|Theorem\\|Corollary")))))

;; copied from elixir-smie.el
(defun coq-smie--same-line-as-parent (parent-pos child-pos)
  "Return non-nil if PARENT-POS is on same line as CHILD-POS."
  (= (line-number-at-pos parent-pos) (line-number-at-pos child-pos)))

(defcustom coq-indent-basic nil
  "Basic indentation step.
If nil, default to `proof-indent' if it exists or to `smie-indent-basic'."
  :group 'coq-mode
  :type '(choice (const :tag "Fallback on global settings" nil)
          integer))


;; Debugging smie parent token, needs the highlight library
;;and something like this in .emacs:
;; (require 'highlight)
;; (custom-set-faces '(highlight ((((type x) (class color) (background light)) (:background "Wheat")))))
(defun coq-show-smie--parent (parent token parent-token &optional num msg)
  (ignore-errors
   (message "%s token: %S ; parent: %S ; parent-token: %S" msg token parent parent-token)
   (hlt-unhighlight-region)
   (let* ((beg (if (listp (car parent)) (caar parent) (car parent)))
          (end (cadr parent))
          (regi (list (list beg end)))
          (tok (caddr parent))
          (face (cond
                 ((equal num 1) 'hlt-regexp-level-1)
                 ((equal num 2) 'hlt-regexp-level-2)
                 (t 'hlt-regexp-level-1))))
     (and parent (hlt-highlight-regions regi face)))))



(defun coq-smie-rules (kind token)
  "Indentation rules for Coq.  See `smie-rules-function'.
KIND is the situation and TOKEN is the thing w.r.t which the rule applies."
  (pcase kind
     (`:elem (pcase token
	       (`basic (or coq-indent-basic
                           (bound-and-true-p proof-indent)
                           smie-indent-basic))))
     (`:close-all t)
     (`:list-intro
      (or (member token '("fun" "forall" "quantif exists" "with"))
	  ;; We include "." in list-intro for the ". { .. } \n { .. }" so the
	  ;; second {..} is aligned with the first rather than being indented as
	  ;; if it were an argument to the first.
	  ;; FIXME: this gives a strange indentation for ". { \n .. } \n { .. }"
;	  (when (or (coq-is-bullet-token token)
;		    (coq-is-dot-token token)
;		    (member token '("{ subproof")))
;	    (forward-char 1) ; skip de "."
;	    (equal (coq-smie-forward-token) "{ subproof"))
	  ))
     (`:after
      ;;(coq-show-smie--parent smie--parent smie--token (smie-indent--parent) 1 "AFTER")
      (cond
       ;; Override the default indent step added because of their presence
       ;; in smie-closer-alist.
       ((or (coq-is-bullet-token token)
	    (member token '(":" ":=" ":= with" ":= def" ":= equations"
			    "by" "in tactic" "<:" "<+" ":= record"
			    "with module" "as" ":= inductive" ":= module" )))
	2)

       ((equal token "with match") coq-match-indent)

       ;; Inductive foo ...
       ;; ...
       ;; with
       ;;   bar  <-- indent this by 2
       ;; TODO: have this optional?
       ((equal token "with inductive")
	(if (smie-rule-parent-p "with inductive")
	    0
	  2)) 

       ((equal token "with") 2)  ; add 2 to the column of "with" in the children

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;XXXXXXXXXXXXXXXXXXXXXXXx
       ((and (member token '("{" "{|"))
             (smie-rule-prev-p ":=" ":= def")
             (not coq-indent-box-style))
        (save-excursion
          (smie-backward-sexp t)
          (cons 'column (+ 2 (smie-indent-virtual)))))


       ;;; the ";" tactical ;;;
       ;; ";" is a usual operator, with no indentation
       ;; it would be like this;
       ;;  foo.
       ;;  foo ;
       ;;  bar ;
       ;;  bar.
       ;;  foo.
       ;; which is confusing. So we indent the first ";" of a sequence
       ;; if not already inside an ltacdef of parenthesized tactic:
       ;; foo ;
       ;;   tac ; <--- indented
       ;;   tac ; <--- no indent
       ;;   [ tac2 ;
       ;;     tac1 ; <-- no indentation here
       ;;     now ( tac3 ;  <- neither here
       ;;           tac5) ;
       ;;   ]
       ((equal token "; tactic")
	(if (and (smie-rule-hanging-p)
		 coq-indent-semicolon-tactical
		 (not (coq-smie-is-ltacdef))
		 (not (coq-smie-is-inside-parenthesized-tactic))
		 (or (not (smie-rule-parent-p "; tactic"))
                     ;; FIXME: Don't depend on SMIE's internals!
		     (and (boundp 'smie--parent)
                          smie--parent
			  (coq-smie--same-line-as-parent
			   (nth 1 smie--parent) (point)))))
	    coq-indent-semicolon-tactical
	  nil))

       ((member token '("in let" "in monadic")) (smie-rule-parent))

       ((equal token "} subproof")
        (smie-rule-parent))

       ;; proofstart is a special hack, since "." should be used as a
       ;; separator between commands, here it is recognized as an open
       ;; parenthesis, hence the current command (C) ending with "."
       ;; is not recognized as correctly terminated. The "parent"
       ;; computed by smie is therefore wrong and default indetation
       ;; is broken. We fix this by indenting from the real-start of
       ;; the command terminated by ". proofstart".
       ((equal token ". proofstart")
	(save-excursion (forward-char -1) (coq-find-real-start)
			`(column . ,(+ coq-indent-proofstart (current-column)))))
       ((equal token ". modulestart")
	(save-excursion (forward-char -1) (coq-find-real-start)
			`(column . ,(+ coq-indent-modulestart (current-column)))))))

     (`:before
      ;(coq-show-smie--parent smie--parent smie--token (smie-indent--parent) 2 "BEFORE")
      (cond


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;XXXXXXXXXXXXXXXXXXXXXXXx;;;;;;;;;;;;;;
       ;; trying to indent "{" at the end of line for records, but the
       ;; parent is not what I think.
       ;; ((and (member token '("{" "{|"))
       ;;       (not coq-indent-box-style))
       ;;  (if (smie-rule-bolp) 2 0))
       ;(and (zerop (length tok)) (member (char-before) '(?\{ ?\})))
       ((and (zerop (length token))
             (looking-back "}") ;; "|}" useless when looking backward
             (not coq-indent-box-style))
        (smie-backward-sexp)
        (smie-backward-sexp t)
        (smie-indent-virtual)
        )

       ;; "with" is also in the :list-intro rules and in :after.
       ((equal token "with")
        ;; Hack: We know that "with" is linked to the first word of
        ;; the current atomic tactic. This tactic is the parent, not
        ;; the "." of the previous command.
        `(column . ,(+ 2 (coq-find-with-related-backward))))

       ((equal token "with module")
	(if (smie-rule-parent-p "with module")
	    (smie-rule-parent)
	  (smie-rule-parent 2)))

       ((member token '("in tactic" "as" "by"))
	(cond
	 ((smie-rule-parent-p "- bullet" "+ bullet" "* bullet"
			      "-- bullet" "++ bullet" "** bullet"
			      "--- bullet" "+++ bullet" "*** bullet"
			      "---- bullet" "++++ bullet" "**** bullet"
			      "{ subproof" ". proofstart")
	  (smie-rule-parent 4))
	 ((smie-rule-parent-p "in tactic") (smie-rule-parent))
	 (t (smie-rule-parent 2))))

       ((equal token "as")
	(if (smie-rule-parent-p "in tactic") (smie-rule-parent) 2))

       ((equal token "as morphism") (smie-rule-parent 2))
       ((member token '("xxx provedby" "with signature"))
	(if (smie-rule-parent-p "xxx provedby" "with signature")
	    (smie-rule-parent)
	  (smie-rule-parent 4)))


       ;; This applies to forall located on the same line than "Lemma"
       ;; & co. This says that "if it *were* be on the beginning of
       ;; line" (which it is not) it would be indented of 2 wrt
       ;; "Lemma". This never applies directly to indent the forall,
       ;; but it is used to guess indentation of the next line. This
       ;; allows fo the following indentation:
       ;;  Lemma foo: forall x:nat,
       ;;      x <= 0 -> x = 0.
       ;; which refer to:
       ;;  Lemma foo:
       ;;    forall x:nat, <--- if it where on its own line it would be on column 2
       ;;      x <= 0 -> x = 0. <--- therefore this is on column 4.
       ;; instead of:
       ;;  Lemma foo: forall x:nat,
       ;;               x <= 0 -> x = 0.

       ((and (member token '("forall" "quantif exists"))
	     (not coq-indent-box-style)
	     (not (smie-rule-bolp)))
	(smie-rule-parent coq-smie-after-bolp-indentation))



       ((and (member token '("forall" "quantif exists"))
	     (smie-rule-parent-p "forall" "quantif exists"))
	(if (save-excursion
	      (coq-smie-search-token-backward '("forall" "quantif exists"))
	      (equal (current-column) (current-indentation)))
	    (smie-rule-parent)
	  (smie-rule-parent 2)))


       ;; This rule allows "End Proof" to align with corresponding ".
       ;; proofstart" PARENT instead of ". proofstart" itself
       ;;  Typically:
       ;;    "Proof" ". proofstart"
       ;;    "Qed" <- parent is ". proofstart" above
       ;; Align with the real command start of the ". xxxstart"
       ((member token '(". proofstart" ". modulestart"))
	(save-excursion (coq-find-real-start)
			`(column . ,(current-column))))

       ((or (member token '(":= inductive" ":= def" ":= equations" ":="))
            (and (equal token ":") (smie-rule-parent-p ".")))
        (let ((pcol
               (save-excursion
                 ;; Indent relative to the beginning of the current command
                 ;; rather than relative to the previous command.
                 (smie-backward-sexp token)
                 ;; special case: if this ":=" corresponds to a "with
                 ;; foo", then the previous smie-backward-sexp stopped
                 ;; between "with" and "foo" (because "with inductive"
                 ;; and co are considered as ".", maybe this is the
                 ;; problem), but we want to indent from the column of
                 ;; "with" instead
                 (let ((col1 (current-column)))
                   (if (equal (coq-smie-backward-token) "with inductive")
                       (current-column)
                     col1)
                   ))))
          `(column . ,(if (smie-rule-hanging-p) pcol (+ 2 pcol)))))

       ((equal token "|")
	(cond
         ;; ":= equations" and "; record" are for Equations plugin
         ((smie-rule-parent-p "with match" ":= equations" "; record")
	  (- (funcall smie-rules-function :after "with match") 2))
         ;; This is also for Equations plugijns, but happens at first
         ;; line if a pattern matching and it is ugly to have the "|"
         ;; at the saem column than "{"
         ((smie-rule-parent-p "{")
	  (funcall smie-rules-function :after "with match"))
	 ((smie-rule-prev-p ":= inductive")
	  (- (funcall smie-rules-function :after ":= inductive") 2))
	 (t (smie-rule-separator kind))))))
     ))

;; No need of this hack anymore?
;;       ((and (equal token "Proof End")
;;             (smie-rule-parent-p "Module" "Section" "goalcmd"))
;;        ;; ¡¡Major gross hack!!
;;        ;; This typically happens when a Lemma had no "Proof" keyword.
;;        ;; We should ideally find some other way to handle it (e.g. matching Qed
;;        ;; not with Proof but with any of the keywords like Lemma that can
;;        ;; start a new proof), but we can workaround the problem here, because
;;        ;; SMIE happened to decide arbitrarily that Qed will stop before Module
;;        ;; when parsing backward.
;;        ;; FIXME: This is fundamentally very wrong, but it seems to work
;;        ;; OK in practice.
;;        (smie-rule-parent 2))




(provide 'coq-smie)
;;; coq-smie.el ends here
