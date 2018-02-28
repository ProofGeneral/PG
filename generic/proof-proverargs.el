;;; proof-proverargs.el --- prover command-line arguments

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel
;; Portions © Copyright 2016-2018  Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'pg-user)			; proof-script, new-command-advance

(defun prover-command-line-and-names ()
  (when proof-prog-name-ask
      (let ((prog-name (locate-file proof-prog-name exec-path exec-suffixes 1)))
	(setq proof-prog-name (read-shell-command "Run process: "
						  prog-name))))
  (let* ((prog-name-list1
	  (if (functionp (proof-ass-sym prog-args))
	      ;; complex assistants define <PA>-prog-args as function
	      ;; that computes the argument list.
	      (cons proof-prog-name (funcall  (proof-ass-sym prog-args)))
	    (if (proof-ass prog-args)
		;; Intermediate complex assistants set the value
		;; of <PA>-prog-args to the argument list.
		(cons proof-prog-name (proof-ass prog-args))
	      ;; Trivial assistants simply set proof-prog-name
	      (split-string proof-prog-name))))
	 (prog-name-list
	  ;; Splice in proof-rsh-command if it's non-nil
	  (if (and proof-rsh-command
		   (> (length proof-rsh-command) 0))
	      (append (split-string proof-rsh-command)
		      prog-name-list1)
	    prog-name-list1))
	 (prog-command-line (mapconcat 'identity prog-name-list " ")))
    (cons prog-name-list prog-command-line)))

(provide 'proof-proverargs)

