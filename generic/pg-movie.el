;;; pg-movie.el --- Export a processed script buffer for external replay

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
;; Given a processed proof script, write out an XML file that
;; contains the buffer contents and the contents of prover
;; output attached to spans.
;;
;; See etc/proviola and http://mws.cs.ru.nl/proviola/
;;
;; Much more could be done to dump the prettified output from the
;; prover, but this is probably not the right way of doing things in
;; general (one would rather have prover-integrated batch tools).
;;
;; It does give quick and easy results for provers already supported by
;; Proof General though!
;;

;;; Code:
(eval-when-compile
  (require 'span)
  (require 'unicode-tokens))

(require 'pg-xml)

(defconst pg-movie-xml-header "<?xml version=\"1.0\"?>")

(defconst pg-movie-stylesheet
  "<?xml-stylesheet type=\"text/xsl\" href=\"proviola-spp.xsl\"?>")

(defun pg-movie-stylesheet-location ()
  (concat proof-home-directory "etc/proviola/proviola-spp.xsl"))
  

(defvar pg-movie-frame 0 "Frame counter for movie.")

(defun pg-movie-of-span (span)
  "Render annotated SPAN in XML."
  (let* ((tokens   (proof-ass unicode-tokens-enable))
	 (cmd      (buffer-substring-no-properties
		      (span-start span) (span-end span)))
	 (tcmd     (if tokens 
		       ;; no subscripts of course
		       (unicode-tokens-encode-str cmd)
		     cmd))
	 (helpspan (span-property span 'pg-helpspan))
	 (resp     (when helpspan 
		     (span-property helpspan 'response)))
	 (tresp    (if resp
		       (if tokens 
			   (unicode-tokens-encode-str resp)
			 resp)
		     ""))
	 (type     (span-property span 'type))
	 (class    (cond
		    ((eq type 'comment) "comment")
		    ((eq type 'proof) "lemma")
		    ((eq type 'goalsave) "lemma")
		    (t "command")))
	 (label    (span-property span 'rawname))
	 (frameid  (int-to-string pg-movie-frame)))
    (incf pg-movie-frame)
    (pg-xml-node frame
		 (list (pg-xml-attr frameNumber frameid))
		 (list
		  (pg-xml-node command
			       (append
				(list (pg-xml-attr class class))
				(if label (list (pg-xml-attr label label))))
			       (list tcmd))
		  (pg-xml-node response nil (list tresp))))))

(defun pg-movie-of-region (start end)
  (list (pg-xml-node movie nil
     (list (pg-xml-node film nil
	(span-mapcar-spans-inorder
	 'pg-movie-of-span start end 'type))))))

;;;###autoload
(defun pg-movie-export (&optional force)
  "Export the movie file from the current script buffer.
If FORCE, overwrite existing file without asking."
  (interactive "DP")
  (setq pg-movie-frame 0)
  (let ((movie (pg-movie-of-region
		(point-min)
		(point-max)))
	(movie-file-name
	 (concat 
	  (file-name-sans-extension
		  (buffer-file-name)) ".xml")))

    (with-current-buffer
	(get-buffer-create "*pg-movie*")
      (erase-buffer)
      (insert pg-movie-xml-header "\n")
      (insert pg-movie-stylesheet "\n")
      (xml-print movie)
      (write-file movie-file-name (not force)))))

;;;###autoload
(defun pg-movie-export-from (script &optional force)
  "Export the movie file that results from processing SCRIPT."
  (interactive "fFile: 
P")
  (let ((proof-full-annotation t) ; dynamic
	(proof-fast-process-buffer t))
    (find-file script)
    (proof-process-buffer)
    (pg-movie-export force)))
     
;;;###autoload
(defun pg-movie-export-directory (dir extn)
  "Export movie files from directory DIR with extension EXTN.
Existing XML files are overwritten."
  (interactive "DDirectory: 
sFile extension: ")
  (let ((files (directory-files 
		dir t 
		(concat "\\." extn "$"))))
    (dolist (f files)
      (pg-movie-export-from f 'force))
    (copy-file (pg-movie-stylesheet-location)
	       dir 'overwrite)))


(provide 'pg-movie)

;;; pg-movie.el ends here
