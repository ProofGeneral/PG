;;; proof.el --- Proof General theorem prover interface.

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

;;; Commentary:
;;
;; This file loads Proof General.  It is required by the
;; individual prover modes.  Loading order of PG is:
;;
;; 1. proof-site (variables, autoloads & stubs for mode functions)
;; 2. stub <PA>-mode function sets proof-assistant-symbol and related variables
;; 3. prover-dependent variables defined in pg-custom
;; 4. stub explicitly loads <PA>/<PA>.el and execute real mode function
;; 5. <PA>.el requires this file, rest of PG loaded here
;; 6. further modules loaded by autoloads/prover-specific requires.
;;
;;
;;; Code:

(require 'proof-site)			; site/prover config, global vars, autoloads

(unless (or noninteractive (bound-and-true-p byte-compile-current-file))
  (proof-splash-message))		; welcome the user now.

(require 'proof-compat)			; Emacs and OS compatibility
(require 'proof-utils)			; utilities
(require 'proof-config)			; configuration variables
(require 'proof-auxmodes)		; auxmode functions
(require 'proof-script)			; script mode
(require 'proof-tree)			; proof tree visualization with prooftree
(require 'proof-server)			; server functionality

(provide 'proof)
;;; proof.el ends here
