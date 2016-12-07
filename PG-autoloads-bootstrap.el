;;;###autoload
;;; Bootstrap ProofGeneral's autoloads.
;; The code in package.el will not look in sub-directories of "the package directory"
;; \(c.f. `package-generate-autoloads')
;; , so it needs a little help from the code below to find ProofGeneral's autoloads.
(let* ((base-dir (file-name-directory
                  (if load-in-progress load-file-name
                    buffer-file-name)))
       (autoloads-addendum (expand-file-name "PG-autoloads-addendum.el" base-dir)))
  (unless (file-exists-p autoloads-addendum)
    (let ((generated-autoload-file autoloads-addendum))
      (dolist (name (directory-files-recursively base-dir "" t))
        (when (file-directory-p name)
          (update-directory-autoloads name)))))
  ;; the load below is a wart, but it doesn't look I get full functionality if I just rely on autolads
  (load (expand-file-name "generic/proof-site" base-dir) t t)
  (with-demoted-errors "Error in ProofGeneral's autoloads: %s"
    (load autoloads-addendum t t)))
