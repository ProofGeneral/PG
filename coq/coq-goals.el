;;; coq-goals.el -- code for formatting, displaying goals

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012, David Aspinall and University of Edinburgh
;; Portions © Copyright 1985-2014, Free Software Foundation, Inc
;; Portions © Copyright 2001-2006, Pierre Courtieu
;; Portions © Copyright 2010, Erik Martin-Dorel
;; Portions © Copyright 2012, Hendrik Tews
;; Portions © Copyright 2017, Clément Pit-Claudel
;; Portions © Copyright 2016-2017, Massachusetts Institute of Technology

;; Proof General is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 2.

;; Proof General is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Proof General. If not, see <http://www.gnu.org/licenses/>.

(require 'pg-goals)

(require 'coq-xml)
(require 'coq-response)

;; actual definition in coq.el, prevent compiler warning
(defvar coq-prefer-top-of-conclusion)

;;; goal formatting

(defun coq-goals--goal-id (goal)
  (coq-xml-body1 (nth 2 goal)))

(defun coq-goals--goal-hypotheses-8.5 (goal-hypos)
  (coq-xml-body goal-hypos))

(defun coq-goals--goal-hypotheses-8.6 (goal-hypos)
  (let ((richpp-hypos
	 (cl-remove-if 'null
		       (mapcar (lambda (hy) (coq-xml-at-path hy '(richpp (_))))
			       (coq-xml-body goal-hypos)))))
    (mapcar (lambda (rhy) `(_ nil ,(coq-xml-flatten-pp (coq-xml-body rhy))))
	    richpp-hypos)))

(defun coq-goals--goal-hypotheses (goal)
  (let ((goal-hypos (nth 3 goal)))
    (pcase (coq-xml-protocol-version)
      ((pred coq-xml-protocol-8.5-p)
       (coq-goals--goal-hypotheses-8.5 goal-hypos))
      ((pred coq-xml-protocol-8.6-or-later-p)
       (coq-goals--goal-hypotheses-8.6 goal-hypos))
      (_ (coq-xml-bad-protocol)))))

(defun coq-goals--goal-goal-8.5 (goal-goal) 
  (coq-xml-body1 goal-goal))

(defun coq-goals--goal-goal-8.6 (goal-goal)
  (let ((richpp-goal (coq-xml-at-path goal-goal '(richpp (_)))))
    (and richpp-goal
	 (coq-xml-flatten-pp (coq-xml-body richpp-goal)))))

(defun coq-goals--goal-goal (goal)
  (let ((goal-goal (nth 4 goal)))
    (pcase (coq-xml-protocol-version)
      ((pred coq-xml-protocol-8.5-p)
       (coq-goals--goal-goal-8.5 goal-goal))
      ((pred coq-xml-protocol-8.6-or-later-p)
       (coq-goals--goal-goal-8.6 goal-goal))
      (_ (coq-xml-bad-protocol)))))

(defvar goal-indent-length 1)
(defvar goal-indent (make-string goal-indent-length ?\s))

;; length of goal or hypothesis is maximum length of each of its lines
;; lines within a goal or hypothesis are separated by newlines
(defun coq-goals--hyps-or-goal-width (hyps-or-goal)
  (apply 'max (mapcar 'string-width (split-string hyps-or-goal "\n"))))

(defun coq-goals--hyps-sep-width (hyps)
  (if (null hyps)
      0
    (apply 'max (mapcar 'coq-goals--hyps-or-goal-width hyps))))

;; make a pretty goal 

;; Unicode!
(defvar coq-goals--impl-bar-char ?―)

(defvar coq-goals--impl-bar-min-width 5)

(defun coq-goals--format-goal-with-hypotheses (goal hyps)
  (let* ((nl "\n")
	 (nl-indent (concat nl goal-indent))
	 (padding-len 1) ; on either side of hypotheses or goals
	 (padding (make-string padding-len ?\s))
	 (hyps-text (mapcar 'coq-xml-body1 hyps))
	 (formatted-hyps (mapconcat 'identity hyps-text (concat nl-indent padding)))
	 (hyps-width (coq-goals--hyps-sep-width hyps-text))
	 (goal-width (coq-goals--hyps-or-goal-width goal))
	 (width (max coq-goals--impl-bar-min-width (+ (max hyps-width goal-width) (* 2 padding-len))))
	 (goal-offset (/ (- width goal-width) 2))
	 (indented-goal (replace-regexp-in-string
			 "\n"
			 (concat "\n" (make-string (+ goal-indent-length goal-offset) ?\s))
			 goal)))
    (concat goal-indent padding formatted-hyps nl                      ; hypotheses
	    goal-indent (make-string width coq-goals--impl-bar-char) nl           ; implication bar
            goal-indent (make-string goal-offset ?\s) indented-goal))) ; the goal

(defun coq-goals--format-goal-no-hypotheses (goal)
  (concat goal-indent goal))

;; invariant: goals is non-empty
;; actual-num-goals used for actual number of goals when we're hiding subgoals
(defun coq-goals-make-goals-string (goals &optional actual-num-goals hide-first-context kind-of-goals)
  (let* ((num-goals (or actual-num-goals (length goals)))
	 (goal1 (car goals))
	 (goals-rest (if hide-first-context goals (cdr goals)))
	 (goal-counter 1))
    (with-temp-buffer
      (when kind-of-goals
	(insert (format "*** %s goals ***\n" kind-of-goals)))
      (if (= num-goals 1)
	  (insert "1 subgoal\n")
	(insert (format "%d subgoals\n" num-goals)))
      (unless hide-first-context
	(insert (format "\nsubgoal 1 (ID %s):\n" (coq-goals--goal-id goal1)))
	(insert (coq-goals--format-goal-with-hypotheses 
		 (coq-goals--goal-goal goal1)
		 (coq-goals--goal-hypotheses goal1)))
	(when goals-rest
	  (insert "\n")))
      (dolist (goal goals-rest)
	(cl-incf goal-counter)
	(insert (format "\nsubgoal %s (ID %s):\n" goal-counter (coq-goals--goal-id goal)))
	(insert (coq-goals--format-goal-no-hypotheses 
		 (coq-goals--goal-goal goal))))
      (buffer-string))))

;; display of goals

(defun coq-goals--show-first-goal ()
  "Scroll the goal buffer so that the first goal is visible.
   First goal is displayed on the bottom of its window, maximizing the
   number of hypothesis displayed, without hiding the goal"
  (interactive)
  (let ((pg-frame (car (coq-find-threeb-frames)))) ; selecting the good frame
    (with-selected-frame (or pg-frame (window-frame (selected-window)))
      ;; prefer current frame
      (let ((goal-win (or (get-buffer-window proof-goals-buffer) (get-buffer-window proof-goals-buffer t)))
	    ;; the separator character may have been overridden
	    (separator (make-string coq-goals--impl-bar-min-width coq-goals--impl-bar-char)))
	(if goal-win
	    (with-selected-window goal-win
	      ;; find second goal or buffer end, if not found this goes to the
	      ;; end of buffer
	      (search-forward-regexp "subgoal 2\\|\\'")
	      (beginning-of-line)
	      ;; find something backward else than a space: end of conclusion
	      (ignore-errors (search-backward-regexp "\\S-"))
	      (recenter (- 1)) ; put bottom of conclusion at bottom of window
	      (beginning-of-line)
	      ;; if the top of conclusion is hidden we may want to show it instead
	      ;; of bottom of conclusion
	      (when (and coq-prefer-top-of-conclusion
			 ;; return nil if line separator is not visible
			 (not (save-excursion (re-search-backward separator (window-start) t))))
		(re-search-backward separator nil t)
		(recenter 0))
	      (beginning-of-line)))))))

(defun coq-goals-clear-goals-buffer ()
  (pg-goals-display "" t) ; keep response, not showing new goal
  (coq-optimise-resp-windows-if-option))

(defun coq-goals-show-goals (goals)
  (pg-goals-display goals nil)
  (coq-optimise-resp-windows-if-option)
  (coq-goals--show-first-goal))

(provide 'coq-goals)
