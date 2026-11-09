;;; coq-test-without-pg.el --- Test Coq mode without Proof General  -*- lexical-binding: t -*-
;;
;; This file is part of Proof General.
;;
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)

;;; Commentary:
;;
;; Test the part of Coq mode common to all backends, with the Proof
;; General backend disabled (obsolete `coq-use-pg' nil, so `coq-backend'
;; nil): it must not load the proof shell, and the abbrevs, the
;; terminator command, the query and insertion commands, their keys and
;; the insertion menu must be there.

;;; Code:

;; before loading coq-mode.el, which defines it
(defvar coq-use-pg)
(defvar coq-backend)
(setq coq-use-pg nil)

(require 'proof-site)
(require 'ert)
(require 'cl-lib)
(require 'seq)

(defvar coq-mode-abbrev-table)
(defvar coq-load-path)
(defvar coq-prog-args)
(defvar coq-query-function)
(defvar coq-use-yasnippet)
(defvar coq-goals-count-function)
(defvar coq-par-release-items-function)
(defvar coq-par-failure-function)
(defvar coq-par-abort-function)
(defvar coq-lock-ancestors)
(defvar coq-compile-menu)
(defvar coq-set-search-blacklist-function)
(defvar coq-search-blacklist-current-string)
(defvar coq-insert-menu)
(defvar coq-mode-map)
(defvar coq-all-buffers-commands)
(defvar proof-output-tooltips)
(defvar proof-electric-terminator-enable)
(defvar coq-show-proof-stepwise)
(defvar coq-queries-menu-entries)
(defvar coq-other-queries-menu)
(defvar coq-options-menu)
(defvar coq-abbrevs-menu)
(defvar coq-project-menu-entries)

(declare-function coq-run-query "coq-queries")
(declare-function coq-all-buffers-keymap "coq-queries")
(declare-function proof-output-tooltips-toggle "proof-useropts")
(declare-function proof-electric-terminator-toggle "proof-useropts")
(declare-function coq-show-proof-stepwise-toggle "coq-goals")
(declare-function coq-templates-setup "coq-templates")
(declare-function coq-terminator-insert "coq-mode")
(declare-function coq-About "coq-queries")
(declare-function coq-Check "coq-queries")
(declare-function coq-Show "coq-queries")
(declare-function coq-set-printing-parentheses "coq-queries")
(declare-function coq-ask-printing-width "coq-goals")
(declare-function coq-store-goals-win "coq-goals")
(declare-function coq-store-response-win "coq-goals")
(declare-function coq-insert-intros "coq-queries")
(declare-function coq-goals-count "coq-goals")
(declare-function coq-par-start-compilation "coq-par-compile")
(declare-function coq-change-search-blacklist "coq-queries")
(declare-function coq-insert-as-in-next-command "coq-queries")
(declare-function coq-insert-tactic "coq-templates")
(declare-function coq-insert-tactical "coq-templates")
(declare-function coq-insert-solve-tactic "coq-templates")
(declare-function coq-insert-term "coq-templates")
(declare-function coq-insert-command "coq-templates")
(declare-function coq-insert-requires "coq-templates")
(declare-function coq-insert-section-or-module "coq-templates")
(declare-function coq-end-Section "coq-templates")


;;; utility functions

(defmacro coq-test-without-pg-with-file (&rest body)
  "Run BODY in a buffer visiting a Coq file."
  (declare (indent 0) (debug t))
  `(let ((file (make-temp-file "without_pg" nil ".v" "Definition x := 0.\n"))
         buffer)
     (unwind-protect
         (progn
           (find-file file)
           (setq buffer (current-buffer))
           ,@body)
       (when buffer
         (with-current-buffer buffer
           (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file file))))


;;; tests

(ert-deftest coq-test-without-pg-no-shell ()
  "Coq mode without the Proof General backend loads no proof shell."
  (coq-test-without-pg-with-file
    (should (eq major-mode 'coq-mode))
    ;; the obsolete option gives the default of the new one
    (should (null coq-backend))
    (should-not (featurep 'proof))
    (should-not (featurep 'proof-shell))
    (should-not (featurep 'proof-script))))

(ert-deftest coq-test-without-pg-abbrevs ()
  "The abbrevs of Coq mode are there."
  (coq-test-without-pg-with-file
    (should (eq local-abbrev-table coq-mode-abbrev-table))
    (should (equal (abbrev-expansion "is" coq-mode-abbrev-table) "intros "))
    ;; no yasnippet in the tests, `coq-templates-setup' just says so
    (let ((coq-use-yasnippet nil))
      (coq-templates-setup))))

(ert-deftest coq-test-without-pg-project-file ()
  "The project file sets the load path and the arguments."
  (let* ((dir (file-name-as-directory (make-temp-file "without_pg" t)))
         (file (expand-file-name "a.v" dir))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "_RocqProject" dir)
            (insert "-R theories Foo\n-Q lib \"\"\n-arg -w -arg all\n"))
          (with-temp-file file (insert "Definition x := 0.\n"))
          (find-file file)
          (setq buffer (current-buffer))
          (should (equal coq-load-path
                         `((rec ,(expand-file-name "theories" dir) "Foo")
                           (recnoimport ,(expand-file-name "lib" dir) ""))))
          (should (equal coq-prog-args '("-w" "all")))
          (should-not (featurep 'proof)))
      (when buffer (kill-buffer buffer))
      (delete-directory dir t))))

(ert-deftest coq-test-without-pg-commands ()
  "The common commands and their keys are there, without backend to
run the queries."
  (coq-test-without-pg-with-file
    (should (eq (key-binding ".") #'coq-terminator-insert))
    (should (eq (key-binding (kbd "C-c C-a C-b")) #'coq-About))
    (should (eq (key-binding (kbd "C-c C-a C-c")) #'coq-Check))
    (should (eq (key-binding (kbd "C-c C-a C-s")) #'coq-Show))
    (should (eq (key-binding (kbd "C-c C-a C-w")) #'coq-ask-printing-width))
    (should (eq (key-binding (kbd "C-c C-a r")) #'coq-store-response-win))
    (should (eq (key-binding (kbd "C-c C-a g")) #'coq-store-goals-win))
    (should (eq (key-binding (kbd "C-c C-a C-9")) #'coq-set-printing-parentheses))
    (should (eq (key-binding (kbd "C-c C-a C-i")) #'coq-insert-intros))
    (should (eq (key-binding (kbd "C-c C-a [")) #'coq-insert-as-in-next-command))
    (should (eq (key-binding (kbd "C-c C-a C-(")) #'coq-insert-section-or-module))
    (should (eq (key-binding (kbd "C-c C-a C-)")) #'coq-end-Section))
    (should (eq (key-binding (kbd "C-c C-a C-t")) #'coq-insert-tactic))
    (should (eq (key-binding (kbd "C-c C-a t")) #'coq-insert-tactical))
    (should (eq (key-binding (kbd "C-c C-a !")) #'coq-insert-solve-tactic))
    (should (eq (key-binding (kbd "C-c C-a C-SPC")) #'coq-insert-term))
    (should (eq (key-binding (kbd "C-c C-a C-<return>")) #'coq-insert-command))
    (should (eq (key-binding (kbd "C-c C-a C-r")) #'coq-insert-requires))
    ;; no backend
    (should-not coq-query-function)
    (should-error (coq-run-query "Check x") :type 'user-error)
    (should-error (coq-ask-printing-width) :type 'user-error)
    (should-error (coq-set-printing-parentheses) :type 'user-error)
    (should-error (coq-insert-as-in-next-command) :type 'user-error)
    ;; no goals buffer, nothing to copy
    (coq-store-goals-win)
    (should-not (get-buffer "*response-freeze*"))
    ;; the terminator is just inserted
    (goto-char (point-max))
    (let ((last-command-event ?.))
      (call-interactively (key-binding ".")))
    (should (string-suffix-p "0.\n." (buffer-string)))))

(ert-deftest coq-test-without-pg-insertions ()
  "The insertion commands without query work without backend, and
indent."
  (coq-test-without-pg-with-file
    (goto-char (point-max))
    (insert "Lemma l : True.\nProof.\n")
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "reflexivity")))
      (coq-insert-tactic))
    ;; inserted and indented
    (should (string-suffix-p "Proof.\n  reflexivity" (buffer-string)))
    (insert "\nSection S.\n")
    (coq-end-Section)
    (should (string-suffix-p "Section S.\nEnd S." (buffer-string)))))

(ert-deftest coq-test-without-pg-goals-count-function ()
  "A backend can give the numbers of goals instead of reading them."
  (require 'coq-goals)
  (with-temp-buffer
    (insert "2 goals\n")
    (should (equal (car (coq-goals-count)) 2))
    (setq-local coq-goals-count-function (lambda () '(5 . 3)))
    (should (equal (coq-goals-count) '(5 . 3)))))

(ert-deftest coq-test-without-pg-insert-menu ()
  "The menu of the insertions is defined without backend."
  (coq-test-without-pg-with-file
    (should (equal (car coq-insert-menu) "INSERT"))
    (let ((commands (delq nil (mapcar (lambda (item)
                                        (and (vectorp item) (aref item 1)))
                                      coq-insert-menu)))
          (submenus (mapcar #'car (seq-filter #'consp (cdr coq-insert-menu)))))
      (dolist (command '(coq-insert-intros coq-insert-tactic
                         coq-insert-tactical coq-insert-command
                         coq-insert-term coq-insert-section-or-module
                         coq-insert-requires))
        (should (memq command commands)))
      (should (equal submenus '("Tactics (menu)" "Tacticals (menu)"
                                "Command (menu)" "Term (menu)"))))))

;; compilation of the Requires

(defun coq-test-without-pg-compile-require (dir lib-text)
  "Compile the Require of a library with text LIB-TEXT in DIR.
Without the proof shell, with `coq-par-start-compilation' and local
interface functions.  Return released or failed."
  (with-temp-file (expand-file-name "WpgLib.v" dir)
    (insert lib-text))
  (with-temp-file (expand-file-name "WpgMain.v" dir)
    (insert "Require WpgLib.\n"))
  (let ((buffer (find-file-noselect (expand-file-name "WpgMain.v" dir)))
        result)
    (unwind-protect
        (with-current-buffer buffer
          ;; coq/ is in the load path once a Coq file is visited
          (require 'coq-par-compile)
          (setq-local coq-par-release-items-function
                      (lambda (items _job)
                        (setq result (cons 'released items))))
          (setq-local coq-par-failure-function
                      (lambda () (setq result 'failed)))
          (setq-local coq-par-abort-function
                      (lambda (_buffer) (setq result 'aborted)))
          (setq-local coq-lock-ancestors nil)
          (let ((overlay (make-overlay (point-min) (1- (point-max)))))
            (coq-par-start-compilation
             (list (list overlay (list "Require WpgLib.") nil)))
            (with-timeout (60 (error "Compilation timeout"))
              (while (not result)
                (accept-process-output nil 0.1)))
            (when (consp result)
              (should (eq (car (cadr result)) overlay)))
            (if (consp result) 'released result)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(ert-deftest coq-test-without-pg-compile ()
  "The Requires are compiled without the proof shell."
  (let ((dir (make-temp-file "without_pg_compile" t)))
    (unwind-protect
        (progn
          (should (eq (coq-test-without-pg-compile-require
                       dir "Definition a := 0.\n")
                      'released))
          (should (file-exists-p (expand-file-name "WpgLib.vo" dir)))
          (should (eq (coq-test-without-pg-compile-require
                       dir "Definition a := undefined_constant.\n")
                      'failed))
          (should-not (featurep 'proof-shell))
          (should-not (featurep 'proof-script))
          ;; the menu and its toggle commands
          (should (equal (car coq-compile-menu) "Auto Compilation"))
          (should (commandp 'coq-compile-before-require-toggle)))
      (delete-directory dir t))))

(ert-deftest coq-test-without-pg-search-blacklist ()
  "The search blacklist goes through the function of the backend."
  (coq-test-without-pg-with-file
    (should-error (coq-change-search-blacklist "\"foo_\"") :type 'user-error)
    (let ((coq-search-blacklist-current-string "\"Private_\"")
          args)
      (setq-local coq-set-search-blacklist-function
                  (lambda (old new) (setq args (list old new))))
      (coq-change-search-blacklist "\"foo_\"")
      (should (equal args '("\"Private_\"" "\"foo_\"")))
      (should (equal coq-search-blacklist-current-string "\"foo_\"")))))

;; toggles

(ert-deftest coq-test-without-pg-toggles ()
  "The toggle commands of the options are defined without PG."
  (coq-test-without-pg-with-file
    (require 'proof-useropts)
    (require 'coq-goals)
    (require 'coq-compile-common)
    (dolist (command '(proof-three-window-toggle proof-output-tooltips-toggle
                       proof-autosend-toggle proof-electric-terminator-toggle
                       proof-disappearing-proofs-toggle
                       proof-sticky-errors-toggle
                       proof-omit-proofs-option-toggle
                       coq-show-proof-stepwise-toggle
                       coq-auto-adapt-printing-width-toggle
                       coq-Compile))
      (should (commandp command)))
    (let ((proof-output-tooltips nil)
          (coq-show-proof-stepwise nil))
      (proof-output-tooltips-toggle)
      (should proof-output-tooltips)
      (coq-show-proof-stepwise-toggle)
      (should coq-show-proof-stepwise))
    ;; the electric terminator, its function and its lighter
    (let ((proof-electric-terminator-enable nil))
      (proof-electric-terminator-toggle 1)
      (should proof-electric-terminator-enable)
      (should (stringp (format-mode-line minor-mode-alist)))
      (proof-electric-terminator-toggle 0)
      (should-not proof-electric-terminator-enable))
    (should-not (featurep 'proof-shell))
    (should-not (featurep 'proof-script))))

(defun coq-test-without-pg-menu-commands (entries)
  "Return the commands of the menu items in ENTRIES, and of their submenus."
  (let (commands)
    (dolist (item entries)
      (cond
       ((and (vectorp item) (symbolp (aref item 1)))
        (push (aref item 1) commands))
       ((consp item)
        (setq commands
              (append (coq-test-without-pg-menu-commands (cdr item))
                      commands)))))
    commands))

(ert-deftest coq-test-without-pg-menus ()
  "The menus common to all backends only use commands defined without PG."
  (coq-test-without-pg-with-file
    (should (equal (car coq-other-queries-menu) "OTHER QUERIES"))
    (should (equal (car coq-options-menu) "OPTIONS"))
    (should (equal (car coq-abbrevs-menu) "ABBREVS"))
    (dolist (entries (list coq-queries-menu-entries
                           (cdr coq-other-queries-menu)
                           (cdr coq-options-menu)
                           (cdr coq-abbrevs-menu)
                           coq-project-menu-entries))
      (let ((commands (coq-test-without-pg-menu-commands entries)))
        (should commands)
        (dolist (command commands)
          (should (commandp command)))))
    (should (memq 'coq-ask-printing-width
                  (coq-test-without-pg-menu-commands (cdr coq-options-menu))))
    (should-not (featurep 'proof-shell))))

;; key bindings of the user

(ert-deftest coq-test-without-pg-user-keys ()
  "The key bindings of the user in `coq-mode-map' apply without PG.
Also those of `local-set-key' in `coq-mode-hook', which changes
`coq-mode-map'.  See also `coq-test-queries-user-keys'."
  (let ((hook (lambda () (local-set-key (kbd "C-c z") #'coq-About))))
    (unwind-protect
        (progn
          (define-key coq-mode-map (kbd "C-c y") #'coq-Check)
          (add-hook 'coq-mode-hook hook)
          (coq-test-without-pg-with-file
            (should (eq (current-local-map) coq-mode-map))
            (should (eq (key-binding (kbd "C-c y")) #'coq-Check))
            (should (eq (key-binding (kbd "C-c z")) #'coq-About))
            (should (eq (lookup-key coq-mode-map (kbd "C-c z")) #'coq-About))))
      (remove-hook 'coq-mode-hook hook)
      (define-key coq-mode-map (kbd "C-c y") nil)
      (define-key coq-mode-map (kbd "C-c z") nil))))

(ert-deftest coq-test-without-pg-all-buffers-keys ()
  "The keys of `coq-mode-map' for the goals and response buffers.
Those of `coq-all-buffers-commands', default and of the user, but not
those of the parent of `coq-mode-map' nor of the insertions."
  (let ((parent (keymap-parent coq-mode-map))
        (other (make-sparse-keymap)))
    (unwind-protect
        (coq-test-without-pg-with-file
          (dolist (command coq-all-buffers-commands)
            (should (commandp command)))
          (define-key coq-mode-map (kbd "C-c y") #'coq-Check)
          (define-key other (kbd "C-c w") #'coq-About)
          (set-keymap-parent coq-mode-map other)
          (let ((map (coq-all-buffers-keymap)))
            (should (eq (lookup-key map (kbd "C-c C-a C-b")) #'coq-About))
            (should (eq (lookup-key map (kbd "C-c C-a g")) #'coq-store-goals-win))
            (should (eq (lookup-key map (kbd "C-c y")) #'coq-Check))
            (should-not (lookup-key map (kbd "C-c w")))
            (should-not (lookup-key map (kbd "C-c C-a C-i")))
            ;; insertions in the script buffer
            (should (eq (lookup-key map (kbd "C-c C-a C-r")) #'coq-store-response-win))
            (should (eq (lookup-key map (kbd "C-c C-a C-g")) #'coq-store-goals-win))
            (should (eq (lookup-key coq-mode-map (kbd "C-c C-a C-r"))
                        #'coq-insert-requires))))
      (set-keymap-parent coq-mode-map parent)
      (define-key coq-mode-map (kbd "C-c y") nil))))

;; compatibility files of the files merged into others

(defun coq-test-without-pg-fresh-emacs (form)
  "Evaluate FORM in a new Emacs with Proof General, return its output."
  (with-temp-buffer
    (call-process (expand-file-name invocation-name invocation-directory)
                  nil t nil "-batch" "-q"
                  "-l" (expand-file-name "../../generic/proof-site.el")
                  "--eval" (prin1-to-string form))
    (buffer-string)))

(ert-deftest coq-test-without-pg-compatibility-files ()
  "The old files merged into others can still be required.
With a message, and `with-eval-after-load' of their feature still works
when the file that has their code is loaded."
  (dolist (old '((coq-diffs coq-goals coq-insert-tagged-text)
                 (coq-local-vars coq-project coq-ask-insert-coq-prog-name)
                 (coq-abbrev coq coq-local-vars-list-show-doc)))
    (pcase-let ((`(,feature ,new ,function) old))
      (let ((output (coq-test-without-pg-fresh-emacs
                     `(progn (proof-ready-for-assistant 'coq) ; for coq.el
                             (require ',feature)
                             (princ (format "defined:%S\n" (fboundp ',function)))))))
        (should (string-match-p "defined:t" output))
        (should (string-match-p (format "%s is obsolete" feature) output)))
      (let ((output (coq-test-without-pg-fresh-emacs
                     `(progn (proof-ready-for-assistant 'coq)
                             (with-eval-after-load ',feature
                               (princ "after-load\n"))
                             (require ',new)))))
        (should (string-match-p "after-load" output))
        (should-not (string-match-p "is obsolete" output))))))

(provide 'coq-test-without-pg)

;;; coq-test-without-pg.el ends here
