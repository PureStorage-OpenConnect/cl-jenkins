(in-package :cl-user)
(defpackage :xml-utils
  (:documentation
   "Some utility functions for processing XML parsed by XMLS")
  (:use :cl)
  (:export :xml-path
           :find-all
           :find-first))
(in-package :xml-utils)


(defun xml-path (tree child &rest descendants)
  "Find a subtree of TREE qualified by the path comprising CHILD and
DESCENDANTS.  Signal a SIMPLE-ERROR if no such path exists in the
tree."
  (apply (if (endp descendants) #'identity #'xml-path)
         (xmls:xmlrep-find-child-tag child tree)
         descendants))


(defun find-all (tree tag)
  "Find all subtrees rooted at TAG of TREE"
  (if (consp tree)
      (let ((sub-results (mapcan #'(lambda (x)
                                     (find-all x tag))
                                 (cddr tree))))
        (if (string-equal tag (car tree))
            (cons tree sub-results)
            sub-results))))


(defun find-first (tree tag)
  "Find the first occurrence, in depth first pre-order, of a subtree
rooted at TAG of TREE.  NIL if TAG does not occur in TREE."
  (car (find-all tree tag)))
