;;; coq-mode.el --- Major mode for Coq proof assistant  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014, 2018  Free Software Foundation, Inc.
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

(unless (locate-library "coq-mode")
  (add-to-list 'load-path
               (expand-file-name
                (file-name-directory (or load-file-name buffer-file-name)))))

;; (if t (require 'coq nil 'noerror))
(require 'coq-smie)
(require 'coq-syntax)                   ;For font-lock-keywords

(defgroup coq-mode ()
  "Major mode to edit Coq code."
  :group 'programming)

;; prettify is in emacs > 24.4
;; FIXME: this should probably be done like for smie above.
(defvar coq-may-use-prettify (fboundp 'prettify-symbols-mode))

(defcustom coq-prog-name
  (or (if (executable-find "coqtop") "coqtop")
      (let ((exec-path (append exec-path '("C:/Program Files/Coq/bin"))))
        (executable-find "coqtop"))
      "coqtop")
  "Name of program to run as Coq.
On Windows with latest Coq package you might need something like:
   C:/Program Files/Coq/bin/coqtop.opt.exe
instead of just \"coqtop\".
This must be a single program name with no arguments.  See option
`coq-prog-args' to manually adjust the arguments to the Coq process.
See also `coq-prog-env' to adjust the environment."
  :type 'string
  :group 'coq
  :group 'coq-mode)

(defun get-coq-library-directory ()
  (let ((default-directory
	  (if (file-accessible-directory-p default-directory)
	      default-directory
	    "/")))
    (or (ignore-errors (car (process-lines coq-prog-name "-where")))
	"/usr/local/lib/coq")))

(defconst coq-library-directory (get-coq-library-directory) ;; FIXME Should be refreshed more often
  "The coq library directory, as reported by \"coqtop -where\".")

(defcustom coq-tags (expand-file-name "/theories/TAGS" coq-library-directory)
  "The default TAGS table for the Coq library."
  :type 'string)

(defcustom coq-use-pg t
  "If non-nil, activate the ProofGeneral backend."
  :type 'boolean)

;; prettify is in emacs > 24.4
(defvar prettify-symbols-alist)

(defconst coq-prettify-symbols-alist
  '(;;("not"	. ?¬)
    ("/\\"	. ?∧)
    ;; ("/\\"	. ?⋀)
    ("\\/"	. ?∨)
    ;; ("\\/"	. ?⋁)
    ("forall"	. ?∀)
    ;; ("forall"	. ?Π)
    ("fun"	. ?λ)
    ("exists"	. ?∃)
    ("->"	. ?→)
    ("<-"	. ?←)
    ("=>"	. ?⇒)
    ;; ("~>"	. ?↝) ;; less desirable
    ;; ("-<"	. ?↢) ;; Paterson's arrow syntax
    ;; ("-<"	. ?⤙) ;; nicer but uncommon
    ("::"	. ?∷)
    ))

(defvar coq-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `coq-mode'.")

(defvar coq-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\$ "." st)
    (modify-syntax-entry ?\/ "." st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?\& "." st)
    (modify-syntax-entry ?_  "_" st) ; beware: word consituent EXCEPT in head position
    (modify-syntax-entry ?\' "_" st) ; always word constituent
    (modify-syntax-entry ?∀ "." st)
    (modify-syntax-entry ?∃ "." st)
    (modify-syntax-entry ?λ "." st) ;; maybe a bad idea... lambda is a letter
    (modify-syntax-entry ?\| "." st)

    ;; Should maybe be "_" but it makes coq-find-and-forget (in coq.el) bug,
    ;; hence the coq-with-altered-syntax-table to put "." into "_" class
    ;; temporarily.
    (modify-syntax-entry ?\. "." st)

    (modify-syntax-entry ?\* ". 23n" st)
    (modify-syntax-entry ?\( "()1" st)
    (modify-syntax-entry ?\) ")(4" st)
    st))

;; FIXME, deal with interactive "Definition"
(defvar coq-outline-regexp
  ;;  (concat "(\\*\\|"
  (concat "[ ]*" (regexp-opt
                  '(
                    "Ltac" "Corr" "Modu" "Sect" "Chap" "Goal"
                    "Definition" "Lemm" "Theo" "Fact" "Rema"
                    "Mutu" "Fixp" "Func")
                  t)))

(defvar coq-outline-heading-end-regexp "\\.[ \t\n]")

(defun coq-near-comment-region ()
  "Return a list of the forme (BEG END).
BEG,END being is the comment region near position PT.
Return nil if PT is not near a comment.
Near here means PT is either inside or just aside of a comment."
  (save-excursion
    (let ((pos (point))
          (ppss (syntax-ppss (1- (point)))))
      (unless (nth 4 ppss)
        ;; We're not squarely inside a comment, nor at its end.
        ;; But we may still be at the beginning of a comment.
        (setq ppss (syntax-ppss
                    (+ pos (if (looking-at comment-start-skip) 2 1)))))
      (when (nth 4 ppss)
        (goto-char (nth 8 ppss))
        (list (point)
              (progn (forward-comment 1) (point)))))))


(defun coq-fill-paragraph-function (_n)
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.v\\'" . coq-mode))

(defalias 'coq--parent-mode
  (if coq-use-pg 'proof-mode 'prog-mode))

;;;###autoload
(define-derived-mode coq-mode coq--parent-mode "Coq"
  "Major mode for Coq scripts.

\\{coq-mode-map}"
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
  (set (make-local-variable 'fill-paragraph-function)
       #'coq-fill-paragraph-function)

  ;; TODO (but only for paragraphs in comments)
  ;; (set (make-local-variable 'paragraph-start)  "[ 	]*\\((\\**\\|$\\)")
  ;; (set (make-local-variable 'paragraph-separate) "\\**) *$\\|$")
  ;; (set (make-local-variable 'adaptive-fill-function)
  ;;      #'coq-adaptive-fill-function)

  (setq-local comment-start "(*")
  (setq-local comment-end   "*)")
  (set (make-local-variable 'comment-start-skip)  "(\\*+ *")
  (set (make-local-variable 'comment-end-skip) " *\\*+)")

  (smie-setup coq-smie-grammar #'coq-smie-rules
              :forward-token #'coq-smie-forward-token
              :backward-token #'coq-smie-backward-token)
  (add-hook 'smie-indent-functions #'coq-smie--args nil t)

  ;; old indentation code.
  ;; (require 'coq-indent)
  ;; (setq
  ;;  ;; indentation is implemented in coq-indent.el
  ;;  indent-line-function #'coq-indent-line
  ;;  proof-indent-any-regexp      coq-indent-any-regexp
  ;;  proof-indent-open-regexp     coq-indent-open-regexp
  ;;  proof-indent-close-regexp    coq-indent-close-regexp)
  ;; (set (make-local-variable 'indent-region-function) #'coq-indent-region)

  ;; we can cope with nested comments
  (set (make-local-variable 'comment-quote-nested) nil)

  ;; FIXME: have abbreviation without holes
  ;(if coq-use-editing-holes (holes-mode 1))
  (if (fboundp 'holes-mode) (holes-mode 1))

  ;; Setup Proof-General interface to Coq.
  (if coq-use-pg (coq-pg-setup))

  ;; font-lock
  (setq-local font-lock-defaults '(coq-font-lock-keywords-1))

  ;; outline
  (setq-local outline-regexp coq-outline-regexp)
  (setq-local outline-heading-end-regexp coq-outline-heading-end-regexp)

  ;; tags
  (if (file-exists-p coq-tags)
      (set (make-local-variable 'tags-table-list)
           (cons coq-tags tags-table-list)))

  (set (make-local-variable 'blink-matching-paren-dont-ignore-comments) t)

  (when coq-may-use-prettify
    (set (make-local-variable 'prettify-symbols-alist)
         coq-prettify-symbols-alist)))

(provide 'coq-mode)

;; Local Variables: ***
;; fill-column: 79 ***
;; indent-tabs-mode: nil ***
;; coding: utf-8 ***
;; End: ***

;;; coq-mode.el ends here
