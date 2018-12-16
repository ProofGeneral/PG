;;; phox.el --- Major mode for the PhoX proof assistant

;; This file is part of Proof General.

;; Copyright © 2017  Christophe Raffalli

;;; Commentary:
;; 

(require 'proof-site)

;;; Code:

(proof-ready-for-assistant 'phox)

(require 'proof)
(require 'proof-easy-config)		; easy configure mechanism

(defconst phox-goal-regexp
  "\\(prop\\(osition\\)?\\)\\|\\(lem\\(ma\\)?\\)\\|\\(fact\\)\\|\\(cor\\(ollary\\)?\\)\\(theo\\(rem\\)?\\)")

(proof-easy-config
 'phox "PhoX"
 proof-prog-name		"phox"
 proof-terminal-string          "."
 proof-script-command-end-regexp"[.][ \n\t\r]"
 proof-script-comment-start	"(*"
 proof-script-comment-end	"*)"
 proof-script-syntax-table-entries
   '(?\( "()1"
     ?\) ")(4"
     ?* ". 23"
     ?$ "w"
     ?_ "w"
     ?. "w")
 proof-shell-syntax-table-entries
   '(?\( "()1"
     ?\) ")(4"
     ?* ". 23"
     ?$ "w"
     ?_ "w"
     ?. "w")
 proof-goal-command-regexp	(concat "^" phox-goal-regexp)
 proof-save-command-regexp	"^save"
 proof-goal-with-hole-regexp	(concat "^" phox-goal-regexp "\\(\\([a-zA-Z0-9_$]*\\)\\) ")
 proof-save-with-hole-regexp	"save \\(\\([a-zA-Z0-9_$]*\\)\\).[ \n\t\r]"
 proof-non-undoables-regexp	"\\(undo\\)\\|\\(abort\\)\\|\\(show\\)\\(.*\\)[ \n\t\r]"
 proof-goal-command		"fact \"%s\"."
 proof-save-command		"save \"%s\"."
 proof-kill-goal-command	"abort."
 proof-showproof-command	"show."
 proof-undo-n-times-cmd		"undo %s."
 proof-auto-multiple-files	 t
 proof-shell-cd-cmd		 "cd \"%s\"."
 proof-shell-interrupt-regexp	 "Interrupt."
 proof-shell-start-goals-regexp	 "goal [0-9]+/[0-9]+"
 proof-shell-end-goals-regexp	 "%PhoX%"
 proof-shell-quit-cmd		 "quit."
 proof-assistant-home-page	 "http://www.lama.univ-savoie.fr/~raffalli/phox.html"
 proof-shell-annotated-prompt-regexp "^\\(>PhoX>\\)\\|\\(%PhoX%\\) "
; proof-shell-error-regexp	 "\\*\\*\\*\\|^.*Error:\\|^uncaught exception \\|^Exception- "
 proof-shell-init-cmd		 ""
; proof-shell-proof-completed-regexp "^No subgoals!"
 proof-script-font-lock-keywords
   '("Cst" "Import" "Use" "Sort"
     "new_intro" "new_elim" "new_rewrite"
     "add_path" "author" "cd"
     "claim" "close_def" "def" "del" "documents"
     "depend" "elim_after_intro" "export"
     "edel" "eshow" "flag" "goal" "include"
     "institute" "path" "print_sort" "priority"
     "quit" "save" "search" "tex" "tex_syntax" "title"
     "proposition" "prop" "lemma" "lem" "fact" "corollary" "cor"
     "theorem" "theo"
                                        ; proof command, FIXME: another color
     "abort" "absurd" "apply" "axiom" "constraints"
     "elim" "from" "goals" "intros" "intro" "instance"
     "local" "lefts" "left" "next" "rewrite" "rewrite_hyp"
     "rename" "rmh" "trivial" "slh" "use" "undo" "unfold"
     "unfold_hyp")
   )

;; code for displaying unicode borrowed from
;; Erik Parmann, Pål Drange latex-pretty-symbol
;; cf. https://bitbucket.org/mortiferus/latex-pretty-symbols.el
(require 'cl-lib)

(defun substitute-pattern-with-unicode-symbol (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the Unicode
symbol SYMBOL.
Symbol can be the symbol directly, no lookup needed."
  (interactive)
  (font-lock-add-keywords
   nil
   `((,pattern
      (0 (progn
	   (compose-region (match-beginning 1) (match-end 1)
	   		   ,symbol
	   		   'decompose-region)
	   nil))))))

(defun substitute-patterns-with-unicode-symbol (patterns)
  "Mapping over PATTERNS, calling SUBSTITUTE-PATTERN-WITH-UNICODE for each of the patterns."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode-symbol (car x)
						      (cl-second x)))
          patterns))

(defun phox-symbol-regex (str)
  "Gets a string, e.g. Alpha, returns the regexp matching the escaped
version of it in Phox code, with no chars in [a-z0-9A-Z] after it."
  (interactive "MString:")
  (concat "[^!%&*+,-/:;≤=>@\\^`#|~]\\(" str "\\)[^!%&*+,-/:;≤=>@\\^`#|~]"))

(defun phox-word-regex (str)
  "Gets a string, e.g. Alpha, returns the regexp matching the escaped
version of it in Phox code, with no chars in [a-z0-9A-Z] after it."
  (interactive "MString:")
  (concat "\\b\\(" str "\\)\\b"))


;;Goto http://www.fileformat.info/info/unicode/block/mathematical_operators/list.htm and copy the needed character
(defun phox-unicode-simplified ()
  "Adds a bunch of font-lock rules to display phox commands as
their unicode counterpart"
  (interactive)
  (substitute-patterns-with-unicode-symbol
   (list
    ;;These need to be on top, before the versions which are not subscriptet
    (list (phox-symbol-regex "<=")"≤")
    (list (phox-symbol-regex ">=")"≥")
    (list (phox-symbol-regex "!=")"≠")
    (list (phox-symbol-regex ":<")"∈")
    (list (phox-symbol-regex ":") "∈")
    (list (phox-symbol-regex "/\\\\")"∀")
    (list (phox-symbol-regex "\\\\/")"∃")
    (list (phox-symbol-regex "<->")"↔")
    (list (phox-symbol-regex "-->")"⟶")
    (list (phox-symbol-regex "->")"→")
    (list (phox-symbol-regex "~")"¬")
    (list (phox-symbol-regex "&")"∧")
    (list (phox-word-regex "or")"∨")
    )))

(add-hook 'phox-mode-hook 'phox-unicode-simplified)
(add-hook 'phox-goals-mode-hook 'phox-unicode-simplified)
(add-hook 'phox-response-mode-hook 'phox-unicode-simplified)

(provide 'phox)

;;; phox.el ends here
