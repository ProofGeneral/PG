;;; maths-menu.el --- insert maths characters from a menu  -*- lexical-binding:t; coding: utf-8 -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017  Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Author: Dave Love <fx@gnu.org>
;; Keywords: convenience
;; Version for Proof General modified by David Aspinall, 2007-8.
;; - Hooks added to insert tokenised versions of unicode characters.
;; - Added more characters to the menus.
;; - Define insertion functions following menu names (useful for keybindings)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Defines a minor mode which defines a menu bar item allowing a wide
;; range of maths/technical characters (roughly the LaTeX repertoire)
;; to be inserted into the buffer by selecting menu items.

;; Menu display only works properly under X with Gtk menus and if the
;; menu uses a font with a suitable repertoire.  (Lucid and Motif
;; toolkit menus can't display multilingual text.  I don't know about
;; MS Windows menus.)  It will work with tmm in tty mode iff the tty
;; displays Unicode.  The tmm version (via F10) is also useful under a
;; window system when the menus don't display the characters
;; correctly, but where the symbols have word syntax, tmm won't
;; provide an ASCII selector for them, which can be a pain to use
;; without a mouse.

;; See also the TeX and sgml Quail input modes.  These will probably
;; behave better if you can remember the input sequences.  For
;; instance, this minor mode won't give you the ability to insert into
;; the minibuffer via the menu, though presumably it could be added to
;; the minibuffer menu.

;;; Code:

(defvar maths-menu-filter-predicate (lambda (_char) t)
  "Predicate function used to filter menu elements.")

(defvar maths-menu-tokenise-insert #'insert
  "Function used to insert possibly formatted or escaped character.")

(defun maths-menu-build-menu (spec)
  (let ((map (make-sparse-keymap "Characters")))
    (dolist (pane spec)
      (let* ((name (pop pane))
	     (pane-map (make-sparse-keymap name)))
	(define-key-after map (vector (intern name)) (cons name pane-map))
	(dolist (elt pane)
	  (let ((fname (intern
			(concat "maths-menu-insert-"
				(replace-regexp-in-string " " "-" (cadr elt))))))
	    (fset fname
		  (lambda ()
		    (interactive)
		    (funcall maths-menu-tokenise-insert (car elt))))
	    (define-key-after pane-map
	      (vector (intern (string (car elt)))) ; convenient unique symbol
	      (list 'menu-item
		    (format "%c  (%s)" (car elt) (cadr elt))
		    fname
		    :visible `(funcall maths-menu-filter-predicate ,(car elt))))))))
    map))

(defvar maths-menu-menu
  (maths-menu-build-menu
   '(("Logic"
      (?∧ "and")
      (?∨ "or")
      (?∀ "for all")
      (?∃ "there exists")
      (?∄ "there does not exist")
      (?⊤ "down tack")
      (?⊥ "up tack"))
     ("Binops 1"
      (?± "plus-minus sign")
      (?∓ "minus-or-plus sign")
      (?× "multiplication sign")
      (?÷ "division sign")
      (?− "minus sign")
      (?∗ "asterisk operator")
      (?⋆ "star operator")
      (?○ "white circle")
      (?• "bullet")
      (?· "middle dot")
      (?∩ "intersection")
      (?∪ "union")
      (?⊎ "multiset union")
      (?⊓ "square cap")
      (?⊔ "square cup")
      (?∨ "logical or")
      (?∧ "logical and")
      (?∖ "set minus")
      (?≀ "wreath product"))
     ("Binops 2"
      (?⋄ "diamond operator")
      (?△ "white up-pointing triangle")
      (?▽ "white down-pointing triangle")
      (?◃ "white left-pointing small triangle")
      (?▹ "white right-pointing small triangle")
      (?◁ "white left-pointing triangle")
      (?▷ "white right-pointing triangle")
      (?⊕ "circled plus")
      (?⊖ "circled minus")
      (?⊗ "circled times")
      (?⊘ "circled division slash")
      (?⊙ "circled dot operator")
      (?◯ "large circle")
      (?† "dagger")
      (?‡ "double dagger")
      (?⊴ "normal subgroup of or equal to")
      (?⊵ "contains as normal subgroup or equal to"))
     ("Relations 1"
      (?≤ "less-than or equal to")
      (?≺ "precedes")
      (?≪ "much less-than")
      (?⊂ "subset of")
      (?⊆ "subset of or equal to")
      (?⊏ "square image of")
      (?⊑ "square image of or equal to")
      (?∈ "element of")
      (?∉ "not an element of")
      (?⊢ "right tack")
      (?≥ "greater-than or equal to")
      (?≻ "succeeds")
      (?≽ "succeeds or equal to")
      (?≫ "much greater-than")
      (?⊃ "superset of")
      (?⊇ "superset of or equal to")
      (?⊐ "square original of")
      (?⊒ "square original of or equal to")
      (?∋ "contains as member")
      (?≡ "identical to")
      (?≢ "not identical to") )
     ("Relations 2"
      (?⊣ "left tack")
      (?∼ "tilde operator")
      (?≃ "asymptotically equal to")
      (?≍ "equivalent to")
      (?≈ "almost equal to")
      (?≅ "approximately equal to")
      (?≠ "not equal to")
      (?≐ "approaches the limit")
      (?∝ "proportional to")
      (?⊧ "models")
      (?∣ "divides")
      (?∥ "parallel to")
      (?⋈ "bowtie")
      (?⋈ "bowtie")
      (?⌣ "smile")
      (?⌢ "frown")
      (?≙ "estimates")
      (?⋿ "z notation bag membership"))
     ("Arrows"
      (?← "leftwards arrow")
      (?⇐ "leftwards double arrow")
      (?→ "rightwards arrow")
      (?⇒ "rightwards double arrow")
      (?↔ "left right arrow")
      (?⇔ "left right double arrow")
      (?↦ "rightwards arrow from bar")
      (?↩ "leftwards arrow with hook")
      (?↼ "leftwards harpoon with barb upwards")
      (?↽ "leftwards harpoon with barb downwards")
      (?⇌ "rightwards harpoon over leftwards harpoon")
      (?↦ "rightwards arrow from bar")
      (?↪ "rightwards arrow with hook")
      (?⇀ "rightwards harpoon with barb upwards")
      (?⇁ "rightwards harpoon with barb downwards")
      (?↝ "rightwards wave arrow")
      (?↑ "upwards arrow")
      (?⇑ "upwards double arrow")
      (?↓ "downwards arrow")
      (?⇓ "downwards double arrow")
      (?↕ "up down arrow")
      (?↗ "north east arrow")
      (?↘ "south east arrow")
      (?↙ "south west arrow")
      (?↖ "north west arrow")
      (?⇛ "rightwards triple arrow"))
     ("Long arrows"
      (?⟶ "long rightwards arrow")
      (?⟷ "long left right arrow")
      (?⟸ "long left double arrow")
      (?⟹ "long right double arrow")
      (?⟺ "long left right double arrow")
      (?⟻ "long left arrow from bar")
      (?⟼ "long right arrow from bar")
      (?⟽ "long left double arrow bar")
      (?⟾ "long right double arrow from bar")
      (?⟿ "long rightwards squiggle arrow"))
     ("Symbols 1"
      (?ℵ "alef symbol") ; don't use letter alef (avoid bidi confusion)
      (?ℏ "planck constant over two pi")
      (?ı "latin small letter dotless i")
      (?ℓ "script small l")
      (?℘ "script capital p")
      (?ℜ "black-letter capital r")
      (?ℑ "black-letter capital i")
      (?℧ "inverted ohm sign")
      (?′ "prime")
      (?∅ "empty set")
      (?∇ "nabla")
      (?√ "square root")
      (?∛ "cube root")
      (?∠ "angle")
      (?¬ "not sign")
      (?♯ "music sharp sign")
      (?∂ "partial differential")
      (?∞ "infinity") )
     ("Symbols 2"
      (?□ "white square")
      (?◇ "white diamond")
      (?▵ "white up-pointing small triangle")
      (?∑ "n-ary summation")
      (?∏ "n-ary product")
      (?∐ "n-ary coproduct")
      (?∫ "integral")
      (?∮ "contour integral")
      (?⋂ "n-ary intersection")
      (?⋃ "n-ary union")
      (?⋁ "n-ary logical or")
      (?⋀ "n-ary logical and")
      (?ℕ "double-struck capital n")
      (?ℙ "double-struck capital p")
      (?ℚ "double-struck capital q")
      (?ℝ "double-struck capital r")
      (?ℤ "double-struck capital z")
      (?ℐ "script capital i")
      (?ƛ "latin small letter lambda with stroke")
      (?∴ "therefore")
      (?… "horizontal ellipsis")
      (?⋯ "midline horizontal ellipsis")
      (?⋮ "vertical ellipsis")
      (?⋱ "down right diagonal ellipsis")
      (?⋰ "up right diagonal ellipsis")
      (?⦁ "z notation spot")
      (?⦂ "z notation type colon"))
     ("Delimiters"
      (?\⌊ "left floor")
      (?\⌈ "left ceiling")
      (?\〈 "left-pointing angle bracket")
      (?\⌋ "right floor")
      (?\⌉ "right ceiling")
      (?\〉 "right-pointing angle bracket")
      (?\〚 "left white square bracket")
      (?\〛 "right white square bracket")
      (?\《 "left double angle bracket")
      (?\》 "right double angle bracket")
      (?\⦇ "z notation left image bracket")
      (?\⦈ "z notation right image bracket")
      (?\⦉ "z notation left binding bracket")
      (?\⦊ "z notation right binding bracket"))
     ("Greek LC"
      (?α "alpha")
      (?β "beta")
      (?γ "gamma")
      (?δ "delta")
      (?ε "epsilon")
      (?ζ "zeta")
      (?η "eta")
      (?θ "theta")
      (?ϑ "theta symbol")
      (?ι "iota")
      (?κ "kappa")
      (?λ "lamda")
      (?μ "mu")
      (?ν "nu")
      (?ξ "xi")
      (?π "pi")
      (?ϖ "pi symbol")
      (?ρ "rho")
      (?ϱ "rho symbol")
      (?σ "sigma")
      (?ς "final sigma")
      (?τ "tau")
      (?υ "upsilon")
      (?φ "phi")
      (?ϕ "phi symbol")
      (?χ "chi")
      (?ψ "psi")
      (?ω "omega"))
     ("Greek UC"
      (?Γ "Gamma")
      (?Δ "Delta")
      (?Θ "Theta")
      (?Λ "Lamda")
      (?Ξ "Xi")
      (?Π "Pi")
      (?Σ "Sigma")
      (?Υ "Upsilon")
      (?Φ "Phi")
      (?Ψ "Psi")
      (?Ω "Omega"))
     ("Sub/super"
      (?⁽ "superscript left parenthesis")
      (?⁾ "superscript right parenthesis")
      (?⁺ "superscript plus sign")
      (?⁻ "superscript minus")
      (?⁰ "superscript zero")
      (?¹ "superscript one")
      (?² "superscript two")
      (?³ "superscript three")
      (?⁴ "superscript four")
      (?⁵ "superscript five")
      (?⁶ "superscript six")
      (?⁷ "superscript seven")
      (?⁸ "superscript eight")
      (?⁹ "superscript nine")
      (?₍ "subscript left parenthesis")
      (?₎ "subscript right parenthesis")
      (?₊ "subscript plus sign")
      (?₋ "subscript minus")
      (?₀ "subscript zero")
      (?₁ "subscript one")
      (?₂ "subscript two")
      (?₃ "subscript three")
      (?₄ "subscript four")
      (?₅ "subscript five")
      (?₆ "subscript six")
      (?₇ "subscript seven")
      (?₈ "subscript eight")
      (?₉ "subscript nine")))))

(defvar maths-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [menu-bar maths]
      `(menu-item "Maths" ,maths-menu-menu
		  :help "Menu of maths characters to insert"))
    map))

;;;###autoload
(define-minor-mode maths-menu-mode
  "Install a menu for entering mathematical characters.
Uses window system menus only when they can display multilingual text.
Otherwise the menu-bar item activates the text-mode menu system.
This mode is only useful with a font which can display the maths repertoire."
  :lighter nil)

(provide 'maths-menu)
;;; maths-menu.el ends here
