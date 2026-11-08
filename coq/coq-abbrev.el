;;; coq-abbrev.el --- coq abbrev table and menus for ProofGeneral mode  -*- lexical-binding:t -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2018  Free Software Foundation, Inc.
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

(require 'proof)
(require 'coq-syntax)
(require 'coq-templates)                ; abbrevs and yasnippets
(require 'coq-goals)                    ; menu of the hypotheses
(require 'coq-compile-common)           ; menu of the compilation
(require 'coq-queries)                  ; menus of the queries and options
(require 'coq-project)                  ; menu of the project file

(defun coq-local-vars-list-show-doc ()
  (interactive)
  (describe-variable 'coq-local-vars-doc))


        ;; (if (fboundp 'with-undo-amalgamate) ;; emacs > 29.1
        ;;     (with-undo-amalgamate (insert abbr) (yas-expand))
        ;;   (progn (insert abbr) (yas-expand)))
      


;; The coq menu partly built from tables

;; Common part (script, response and goals buffers)
(defconst coq-menu-common-entries
  `(
      ["Toggle 3 Windows Mode" proof-three-window-toggle
     :style toggle
     :selected proof-three-window-enable
     :help "Toggles the use of separate windows or frames for Coq responses and goals."
     ]
    ("3 Windows mode layout"
     ["smart"
      (progn
	(customize-set-variable 'proof-three-window-mode-policy 'smart)
	(proof-layout-windows))
      :style radio
      :selected (eq proof-three-window-mode-policy 'smart)
      :help "Adapt to frame width (C-c C-l to refresh)"]
     ["hybrid"
      (progn
	(customize-set-variable 'proof-three-window-mode-policy 'hybrid)
	(proof-layout-windows))
      :style radio
      :selected (eq proof-three-window-mode-policy 'hybrid)
      :help "two column mode"]
     ["horizontal"
      (progn
	(customize-set-variable 'proof-three-window-mode-policy 'horizontal)
	(proof-layout-windows))
      :style radio
      :selected (eq proof-three-window-mode-policy 'horizontal)
      :help "Three column mode"]
     ["vertical"
      (progn
	(customize-set-variable 'proof-three-window-mode-policy 'vertical)
	(proof-layout-windows))
      :style radio
      :selected (eq proof-three-window-mode-policy 'vertical)
      :help "One column mode"])
    ["Refresh Windows Layout" proof-layout-windows t]
    ["Toggle tooltips" proof-output-tooltips-toggle
     :style toggle
     :selected proof-output-tooltips
     :help "Toggles Tooltips (popup when hovering commands).\nSet `proof-output-tooltips' to nil to disable it by default."]
    ["Electric Terminator" proof-electric-terminator-toggle
     :style toggle
     :selected proof-electric-terminator-enable
     :help "Automatically send commands when terminator typed"]
    ["Fold Other Goals" coq-toggle-other-goals
     :style toggle
     :selected coq-fold-other-goals
     :help "Display only the number of the goals after the first one."]
    ,coq-hyps-menu
    ["Double Hit Electric Terminator" proof-double-hit-toggle
     :style toggle
     :selected proof-double-hit-enable
     :help "Automatically send commands when terminator is typed twice quickly."]
    ,coq-compile-menu
     ("Diffs"
      ["off"
       (customize-set-variable 'coq-diffs 'off)
       :style radio
       :selected (eq coq-diffs 'off)
       :help "Don't show diffs"]
      ["on"
       (customize-set-variable 'coq-diffs 'on)
       :style radio
       :selected (eq coq-diffs 'on)
       :help "Show diffs: only added"]
      ["removed"
       (customize-set-variable 'coq-diffs 'removed)
       :style radio
       :selected (eq coq-diffs 'removed)
       :help "Show diffs: added and removed"])
    ["Show Proof (Diffs)"
      coq-show-proof-stepwise-toggle
      :style toggle
      :selected coq-show-proof-stepwise
      :help "Display the proof terms stepwise in the *response* buffer."]
    ("\"Proof using\" mode..."
     ["ask"
      (customize-set-variable 'coq-accept-proof-using-suggestion 'ask)
      :style radio
      :selected (eq coq-accept-proof-using-suggestion 'ask)
      :help "Ask user when a new proof using annotation is suggested"]
     ["always"
      (customize-set-variable 'coq-accept-proof-using-suggestion 'always)
      :style radio
      :selected (eq coq-accept-proof-using-suggestion 'always)
      :help "Always update the proof using annotation when suggested"]
     ["highlight"
      (customize-set-variable 'coq-accept-proof-using-suggestion 'highlight)
      :style radio
      :selected (eq coq-accept-proof-using-suggestion 'highlight)
      :help "Highight the proof when an auto insert is suggested (right click to insert))"]
     ["Ignore"
      (customize-set-variable 'coq-accept-proof-using-suggestion 'ignore)
      :style radio
      :selected (eq coq-accept-proof-using-suggestion 'ignore)
      :help "no highlight (right click insertion still possible)"])
    ""
    ,@coq-queries-menu-entries
    ,coq-other-queries-menu
    ,coq-options-menu))

(setq-default coq-menu-entries
  (append coq-menu-common-entries
  `(
    ""
    ,coq-insert-menu
    ""
    ,coq-abbrevs-menu
    ""
    ("COQ PROG (ARGS)"
     ,@coq-project-menu-entries
     ["Set Coq Prog *persistently*" coq-ask-insert-coq-prog-name t]
     ["help" coq-local-vars-list-show-doc t]
     ["Compile" coq-Compile t]))))

(setq-default coq-help-menu-entries
  '(["help on setting prog name persistently for a file"
     coq-local-vars-list-show-doc t]))

(setq-default coq-other-buffers-menu-entries coq-menu-common-entries)

(provide 'coq-abbrev)

;;; coq-abbrev.el ends here
