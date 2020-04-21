;;; coq-tests.el --- integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;;  Eval this to run the tests interactively <C-x C-e>
;;
;; (call-interactively #'ert-run-tests-interactively)

(setq debug-on-error t) ; open the debugger on error -- may be commented-out

; (require 'ert-async)
;(setq ert-async-timeout 2)

;;; Code:

; Exemple de code Lisp qui lance des commandes au prouveur en arrière-plan
; (defun has-error ()
;   "True if error in Proof"
;   (eq 'error proof-shell-last-output-kind))
; 
; (defun test-process-invisible-split ()
;   (proof-shell-invisible-command "split."
;    'waitforit
;     #'proof-done-invisible
;     'no-error-display 'no-response-display 'no-goals-display))
; 
; (defun test-process-invisible-tactics-then-reset-and-insert ()
;   (interactive)
;   (let ((reset-cmd ; store backtracking info before proof search
;            (format "Backtrack %s %s %s . "
;                    (int-to-string coq-last-but-one-statenum)
;                    (int-to-string coq-last-but-one-proofnum)
;                    0)))
;          ;; Toy example of proof search
;          (while (not (has-error))
;                 (message "Trying split.")
;                 (test-process-invisible-split))
;          ;; Reset to the previous state
;          (proof-shell-invisible-command reset-cmd 'waitforit #'proof-done-invisible)
;          ;; Insert (and re-process) the found script
;          (proof-insert-sendback-command "split.\nsplit.\nsplit.\nQed.")))
; 
; (test-process-invisible-tactics-then-reset-and-insert)

(defconst coq-test-file-prefix "coq_test_")

(defun coq-test-init ()
  "Ensure `coq' is loaded."
  (unless (featurep 'coq)
    (add-to-list 'load-path
		 (locate-dominating-file buffer-file-name "proof-general.el"))
    (load "proof-general")
    (proofgeneral "coq")))

(defun coq-test-exit ()
  "Exit the Coq process."
  (proof-shell-exit t))



;; AVOID THE FOLLOWING ERROR:
;; Starting:  -emacs
;; Debugger entered--Lisp error: (wrong-type-argument stringp nil)
;;   file-name-directory(nil)
;;   scomint-exec-1("coq" #<buffer *coq*> nil ("-emacs"))
;;   scomint-exec(#<buffer *coq*> "coq" nil nil ("-emacs"))
;;   scomint-make-in-buffer("coq" nil nil nil "-emacs")
;;   apply(scomint-make-in-buffer "coq" nil nil nil "-emacs")
;;   scomint-make("coq" nil nil "-emacs")
;;   apply(scomint-make ("coq" nil nil "-emacs"))
;;   proof-shell-start()
;;   proof-shell-ready-prover()
(defmacro coq-test-on-file (&rest body)
  "Eval BODY like `progn' after opening a temporary Coq file."
  ;; For more info: https://mullikine.github.io/posts/macro-tutorial/
  `(let ((file (concat (make-temp-file coq-test-file-prefix) ".v")))
     (message "Opening file %s ..." file)
     (save-excursion          
       (let ((buffer (find-file file))
             (res (progn ,@body)))
         (progn (kill-buffer buffer)
                (ignore-errors (delete-file file))
                res)))))
; (pp (macroexpand '(coq-test-on-file (message "OK"))))
; (coq-test-on-file (message (buffer-file-name)) (message "OK") 42)

;; DEFINITION OF MOCKS, SEE `coq-mock' BELOW
;; Another solution would consist in using el-mock, mentioned in:
;; https://www.gnu.org/software/emacs/manual/html_mono/ert.html#Mocks-and-Stubs
(defun mock-proof-display-three-b (&rest rest)
  (message (concat "Skipping proof-display-three-b on input: "
                   (pp-to-string rest)))
  ; Result:
  nil)

;; AVOID THE FOLLOWING ERROR:
;; Hit M-x proof-layout-windows to reset layout
;; Debugger entered--Lisp error: (error "Window #<window 6 on *goals*> too small for splitting")
;;   signal(error ("Window #<window 6 on *goals*> too small for splitting"))
;;   error("Window %s too small for splitting" #<window 6 on *goals*>)
;;   split-window(nil nil)
;;   split-window-vertically()
;;   proof-safe-split-window-vertically()
;;   proof-select-three-b(nil #<buffer *goals*> #<buffer *response*> smart)
;;   proof-display-three-b(smart)
;;   proof-layout-windows()
;;   proof-multiple-frames-enable()
;;   proof-shell-start()
;;   proof-shell-ready-prover()
(defun coq-mock (f)
  (require 'pg-response) ; load the feature defining proof-display-three-b first
   (cl-letf (;((symbol-function 'foo) #'mock-foo)
             ((symbol-function 'proof-display-three-b) #'mock-proof-display-three-b))
     (funcall f)))
;; Run <C-x C-e> on:
;; (coq-mock #'main)

(defun coq-test-cmd (cmd)
  (coq-test-on-file
    (coq-test-init)
    (proof-shell-invisible-command
     cmd
     'waitforit
     #'proof-done-invisible
     'no-error-display 'no-response-display 'no-goals-display)))

; Fixture for init and exit coq
(defun coq-init-exit (body)
       (unwind-protect
           (progn  (coq-test-init)
                  (funcall body))
          (coq-test-exit)))

(defun coq-test-001 ()
  ;; TODO: retrieve the test status, maybe by changing the function above
  (coq-test-cmd (process-list)))
;; TODO: Use https://github.com/rejeep/ert-async.el
;; and/or ERT https://www.gnu.org/software/emacs/manual/html_node/ert/index.html

(ert-deftest coq-test-running ()
  (coq-init-exit
   (lambda () 
  (coq-test-cmd "Check 0.")
  (should  (get-process "coq")))))

(defun coq-test-main ()
  (coq-mock #'coq-test-001))

;(ignore-errors (coq-test-exit))
(coq-test-main)

(provide 'coq-tests)
;;; learn-ocaml-tests.el ends here
