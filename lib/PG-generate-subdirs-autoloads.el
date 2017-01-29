;; A small function to generate autoloads when ProofGeneral is packaged.

;; Assume this file is in directory that's one level below ProofGeneral's package directory.
(defconst PG-autoloads-base-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (if load-in-progress load-file-name
       buffer-file-name)))))

(defconst PG-autoloads-from-subdirs-file
  (expand-file-name "PG-subdirs-autoloads-generated.el" PG-autoloads-base-dir))

(defun PG-generate-subdirs-autoloads ()
  (unless (file-exists-p PG-autoloads-from-subdirs-file)
      (let ((generated-autoload-file PG-autoloads-from-subdirs-file))
        (dolist (name (directory-files-recursively PG-autoloads-base-dir "" t))
          (when (file-directory-p name)
            (update-directory-autoloads name))))))
