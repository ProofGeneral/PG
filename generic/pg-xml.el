;; pg-xml.el --- XML functions for Proof General

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
;; XML functions for Proof General.
;;

(require 'cl-lib)

(require 'xml)

(require 'proof-utils) ;; for pg-internal-warning

(defalias 'pg-xml-error 'error)


;;
;; Elisp format of XML trees (see xml.el)
;;
;;    xml-list   ::= (node node ...)
;;    node       ::= (qname attribute-list . child_node_list)
;;    child_node_list ::= child_node child_node ...
;;    child_node ::= node | string
;;    qname      ::= (:namespace-uri . "name") | "name"
;;    attribute_list ::= ((qname . "value") (qname . "value") ...)
;;                       | nil
;;    string     ::= "..."
;;
;; NB [da]: without namespace aware parsing, qnames are symbols.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parsing function: pg-xml-parse-buffer
;;

;;;###autoload
(defun pg-xml-parse-string (arg)
  "Parse string in ARG, same as pg-xml-parse-buffer."
  (let
      ((tempbuffer (get-buffer-create " *xml-parse*")))
    (with-current-buffer tempbuffer
      (delete-region (point-min) (point-max))
      (insert arg)
      (pg-xml-parse-buffer (current-buffer) 'nomessage))))


(defun pg-xml-parse-buffer (&optional buffer nomsg start end)
  "Parse an XML documment in BUFFER (defaulting to current buffer).
Parsing according to `xml-parse-file' of xml.el.
Optional START and END bound the parse."
  (unless nomsg
    (message "Parsing %s..." (buffer-name buffer)))
  (let ((xml (xml-parse-region (or start (point-min))
			       (or end (point-max))
			       (or buffer (current-buffer))
			       nil)))
      (unless nomsg
	(message "Parsing %s...done" (buffer-name buffer)))
      xml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions for parsing
;;

(defun pg-xml-get-attr (attribute node &optional optional defaultval)
  (let ((val (cdr (assoc attribute (xml-node-attributes node)))))
    (or val
	(if optional
	    defaultval
	  (pg-xml-error "pg-xml-get-attr: Didn't find required %s attribute in %s element"
		 attribute (xml-node-name node))))))

(defun pg-xml-child-elts (node)
  "Return list of *element* children of NODE (ignoring strings)."
  (let ((children (xml-node-children node)))
    (cl-mapcan (lambda (x) (if (listp x) (list x))) children)))

(defun pg-xml-child-elt (node)
  "Return unique element child of NODE."
  (let ((children (pg-xml-child-elts node)))
    (if (= (length children) 1)
	(car children)
      (pg-internal-warning  "pg-xml-child-elt: expected single element child of %s"
			    (xml-node-name node)))))

(defun pg-xml-get-child (child node)
  "Return single element CHILD of node, give error if more than one."
  (let ((children (xml-get-children node child)))
    (if (> (length children) 1)
	 (progn
	   (pg-internal-warning "pg-xml-get-child: got more than one %s child of %s node, ignoring rest"
				child (xml-node-name node))
	   (car children))
      children)))

(defun pg-xml-get-text-content (node)
  "Return the concatenation of all the text children of node NODE."
  (mapconcat (lambda (x) (if (stringp x) x "")) (xml-node-children node) ""))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Producing functions: constructing an XML tree in xml.el format
;;			and converting to a string

(defmacro pg-xml-attr (name val) `(cons (quote ,name) ,val))

(defmacro pg-xml-node (name atts children)
  `(cons (quote ,name) (cons ,atts ,children)))

(defconst pg-xml-header
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")


(defun pg-xml-string-of (xmls)
  "Convert the XML trees in XMLS into a string (without additional indentation)."
  (let* (strs
	 (insertfn    (lambda (&rest args)
			(setq strs (cons (cl-reduce 'concat args) strs)))))
    (dolist (xml xmls)
      (pg-xml-output-internal xml nil insertfn))
    (cl-reduce 'concat (reverse strs))))

;; based on xml-debug-print from xml.el

(defun pg-xml-output-internal (xml indent-string outputfn)
  "Outputs the XML tree using OUTPUTFN, which should accept a list of args.
Output with indentation INDENT-STRING (or none if nil)."
  (let ((tree xml)
	attlist)
    (funcall outputfn (or indent-string "") "<" (symbol-name (xml-node-name tree)))

    ;;  output the attribute list
    (setq attlist (xml-node-attributes tree))
    (while attlist
      (funcall outputfn " ")
      (funcall outputfn (symbol-name (caar attlist)) "=\"" (cdar attlist) "\"")
      (setq attlist (cdr attlist)))

    (setq tree (xml-node-children tree))

    (if tree
	(progn
	  (funcall outputfn ">")
	  ;;  output the children
	  (dolist (node tree)
	    (cond
	     ((listp node)
	      (if indent-string (funcall outputfn "\n"))
	      (pg-xml-output-internal node (if indent-string (concat indent-string "  ")) outputfn))
	     ((stringp node) (funcall outputfn node))
	     (t
	      (error "pg-xml-output-internal: Invalid XML tree"))))

	  (funcall outputfn (if indent-string (concat "\n" indent-string) "")
		   "</" (symbol-name (xml-node-name xml)) ">"))
      (funcall outputfn "/>"))))


(defun pg-xml-cdata (str)
  (concat "<!\\[CDATA\\[" str "\\]"))

(provide 'pg-xml)
;;; pg-xml.el ends here
