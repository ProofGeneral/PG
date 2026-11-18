;;; coq-test-looking-back.el --- Test of the limits of looking back  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Without limit, `looking-back' tries all the positions back to the
;; beginning of the buffer: called for each token by the indentation,
;; it made indenting large scripts quadratic (issue #919).  Test
;; `coq-looking-back', `coq-looking-back-near', and that the
;; indentation never calls `looking-back' without limit.  Without
;; backend: the indentation is the same in all of them.

;;; Code:

;; before loading coq-mode.el, which defines it
(defvar coq-use-pg)
(setq coq-use-pg nil)

(require 'proof-site)
(require 'ert)
(with-temp-buffer (coq-mode))           ; loads it, through its autoload

(defvar coq-goal-selector-regexp)
(defvar coq-looking-back-window)

(ert-deftest coq-looking-back-limit-mandatory ()
  "`coq-looking-back' needs a limit, which may be before the buffer."
  (with-temp-buffer
    (insert "x \\/")
    (should-error (coq-looking-back "\\\\/" nil) :type 'wrong-type-argument)
    (should (coq-looking-back "\\\\/" (- (point) 2)))
    (should (coq-looking-back "x \\\\/" -10))))

(ert-deftest coq-looking-back-near-window ()
  "`coq-looking-back-near' finds a match starting in its window only."
  (with-temp-buffer
    (insert "2:" (make-string 10 ?\s))
    (should (coq-looking-back-near coq-goal-selector-regexp t))
    (should (equal (match-string 0) "2:          "))
    (erase-buffer)
    (insert "2:" (make-string coq-looking-back-window ?\s))
    (should-not (coq-looking-back-near coq-goal-selector-regexp t))))

(defun looking-back-test-script (lemmas)
  "A section of LEMMAS lemmas, with the constructions found by looking back."
  (concat
   "Section S.\n"
   (mapconcat
    (lambda (i)
      (format "Lemma l%d : True /\\ True /\\ True.
Proof.
refine (conj ?[a] (conj ?[b] ?[c])).
[a]:{ Time exact I. }
2:{
Timeout 10 exact I. }
- Redirect \"out\" Check I.
Fail exact 0.
exact I.
Qed.
" i))
    (number-sequence 1 lemmas) "")
   "End S.\n"))

(ert-deftest coq-indent-looking-back-limited ()
  "Indenting a script calls `looking-back' with a limit only."
  (let ((unlimited nil))
    (cl-flet ((record (regexp &optional limit _greedy)
                (unless limit (push regexp unlimited))))
      (advice-add 'looking-back :before #'record)
      (unwind-protect
          (with-temp-buffer
            (coq-mode)
            (insert (looking-back-test-script 20))
            (indent-region (point-min) (point-max))
            ;; the indentation went through the lines
            (goto-char (point-max))
            (forward-line -2)
            (should (= (current-indentation) 2)))
        (advice-remove 'looking-back #'record)))
    (should-not unlimited)))

;;; coq-test-looking-back.el ends here
