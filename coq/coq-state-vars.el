;; coq-state-vars.el -- variables holding global state

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003, 2012, 2014  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel
;; Portions © Copyright 2016-2018  Massachusetts Institute of Technology

;; Author: Paul Steckler

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

(defvar coq-auto-insert-as nil)            ; defpacustom
(defvar coq-time-commands nil)             ; defpacustom
(defvar coq-use-project-file t)            ; defpacustom
(defvar coq-use-editing-holes nil)         ; defpacustom
(defvar coq-hide-additional-subgoals nil)  ; defpacustom

;; the current state id is returned in the last <value> in response to an "Add" command
;; the others are in the <status> in response to a "Status" command

;; NB: state ids are strings that currently in Coq contain numbers
(defvar coq-current-state-id nil)
(defvar coq-edit-id-counter 1)
(defvar coq-route-id-counter 1)

;; goal counts
(defvar coq-num-goals 0)
(defvar coq-num-subgoals 0)

(defvar coq-retract-buffer-state-id nil
  "State id to retract entire buffer.
It's the state id returned after Init command sent.")

(defvar coq-xml-protocol-date nil
  "Protocol version for Coq XML protocol, as an 8-character date")

(defun coq-reset-state-vars ()
  (setq coq-current-state-id coq-retract-buffer-state-id
	coq-edit-id-counter 1
	coq-route-id-counter 1
	coq-num-goals 0
	coq-num-subgoals 0))

(add-hook 'proof-server-restart-hook 'coq-reset-state-vars)

;; we see feedback and value-fail messages twice, once for Goal, again for Status
;; see Bug 4850
;; process each one just once, because they have effects; use table to know if they've been seen
;; to prevent this table from taking too much space, we clear it just as each Add is sent
(defvar coq-error-fail-tbl (make-hash-table :test 'equal))

;; table mapping state ids to spans created by processingin feedbacks
;; we make values weak; spans can be deleted from buffer without necessarily
;;  deleting from this table
(defvar coq-processing-span-tbl (make-hash-table :test 'equal :weakness 'value))

;; table mapping state ids to spans
;; values are weak, because spans can be deleted, as on a retract
(defvar coq-span-state-id-tbl (make-hash-table :test 'equal :weakness 'key-and-value))

;; associate edit ids with spans
;; edit ids are numbers, so don't need to use 'equal as test like we did for state ids
(defvar coq-span-edit-id-tbl (make-hash-table :weakness 'value))

;; associate state ids with spans
;; for a span, this is the state id in the corresponding Add call, NOT the state id later associated
;;  with the span
(defvar coq-span-add-call-state-id-tbl (make-hash-table :test 'equal :weakness 'key-and-value))

;; table maps PG face to a rank governing precedence
(defvar coq-face-rank-tbl (make-hash-table))

;; table of active workers
(defvar coq-worker-status-tbl (make-hash-table :test 'equal))

;; map from state ids to feedback queues
(defvar coq-feedbacks-tbl (make-hash-table :test 'equal :weakness 'value))

(defun coq-reset-tables ()
  (mapc 'clrhash
	(list coq-error-fail-tbl
	      coq-span-state-id-tbl
	      coq-span-edit-id-tbl
	      coq-span-add-call-state-id-tbl
	      coq-feedbacks-tbl)))

(add-hook 'proof-server-restart-hook 'coq-reset-tables)

(provide 'coq-state-vars)

;; end coq-state-vars.el
