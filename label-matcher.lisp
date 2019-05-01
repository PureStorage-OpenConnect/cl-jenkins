(in-package :cl-user)
(defpackage :label-matcher
  (:documentation "A library for matching a set of labels to a label expression.

A label expression has the following grammar:

expr ::= disjunction
disjunction ::= conjunction | conjunction '||' disjunction
conjunction ::= term | term '&&' conjunction
term ::= '!' term | <label> | '(' expr ')'

where <label> is a string literal representing a node label.

Example usage:

(label-matcher:satisfiesp '(\"hody\" \"omg\" \"herp\")
                          (label-matcher:compile-expression \"hody&&omg\"))

NB that compiled expressions are just straight-forward s-expressions.
For example, the label expression:

  \"(sbcl||ccl||allegro) && (!vm||has_bluh)\"

Will compile to:

  (:and (:or \"sbcl\" \"ccl\" \"allegro\")
        (:or (:not \"vm\") \"has_bluh\"))")
  (:use :cl :alexandria)
  (:export :compiler-error
           :lex-error
           :parsing-error
           :compile-expression
           :satisfiesp
           :conjunctive-normal-form
           :minimally-canonical-conjunctive-normal-form))
(in-package :label-matcher)


(defun satisfiesp (labels expression)
  "Tests to see if the set of labels denoted by the list LABELS
satisfies the label expression EXPRESSION."
  (if (stringp expression)
      (member expression labels :test #'string=)
      (if (consp expression)
          (cond ((eq :not (car expression))
                 (not (satisfiesp labels (cadr expression))))
                ((eq :and (car expression))
                 (every (curry #'satisfiesp labels) (cdr expression)))
                ((eq :or (car expression))
                 (some (curry #'satisfiesp labels) (cdr expression)))))))


(defun location-marked-string (text location)
  (let ((text-index (copy-seq text)))
    (loop for i from 0 below (length text-index) for c across text-index do
      (setf (char text-index i)
            (cond ((= i location) #\^)
                  ((cl-ppcre:scan "\\s" (string c)) c)
                  (t #\Space))))
    (format nil "~{~A~%~A~^~%~}"
            (mapcan #'list
                    (cl-utilities:split-sequence #\Newline text)
                    (cl-utilities:split-sequence #\Newline text-index)))))


(define-condition compiler-error (error)
  ((.text :initarg :text)
   (.location :initarg :location)
   (.error-message :initarg :error-message))
  (:report (lambda (condition stream)
             (format stream "Position ~A: ~A~%~%~A"
                     (slot-value condition '.location)
                     (slot-value condition '.error-message)
                     (location-marked-string (slot-value condition '.text)
                                             (slot-value condition '.location))))))


(define-condition lex-error (compiler-error)
  ((.error-message :initform "Invalid token at position")))


(define-condition parsing-error (compiler-error) ())


(let ((label-pattern (cl-ppcre:create-scanner "\\w(\\w|-|\\.|:)*"))
      (blank (cl-ppcre:create-scanner "\\s+"))
      (token-map (alist-hash-table (list (cons "(" :lparensym)
                                         (cons ")" :rparensym)
                                         (cons "!" :notsym)
                                         (cons "&&" :andsym)
                                         (cons "||" :orsym))
                                   :test #'equal)))
  (defun next-token (source)
    (multiple-value-bind (start end) (cl-ppcre:scan blank (getf source :text)
                                                    :start (getf source :location))
      (when (eql start (getf source :location))
        (setf (getf source :location) end)))
    (if (< (getf source :location) (length (getf source :text)))
        (let ((one-char-sym (subseq (getf source :text)
                                    (getf source :location)
                                    (min (1+ (getf source :location))
                                         (length (getf source :text)))))
              (two-char-sym (subseq (getf source :text)
                                    (getf source :location)
                                    (min (+ 2 (getf source :location))
                                         (length (getf source :text))))))
          (cond ((gethash one-char-sym token-map)
                 (incf (getf source :location))
                 (values (gethash one-char-sym token-map) one-char-sym))
                ((gethash two-char-sym token-map)
                 (incf (getf source :location) 2)
                 (values (gethash two-char-sym token-map) two-char-sym))
                ((eql (cl-ppcre:scan label-pattern
                                     (getf source :text)
                                     :start (getf source :location))
                      (getf source :location))
                 (multiple-value-bind (start end)
                     (cl-ppcre:scan label-pattern
                                    (getf source :text)
                                    :start (getf source :location))
                   (setf (getf source :location) end)
                   (values :labelsym
                           (cl-ppcre:scan-to-strings label-pattern
                                                     (getf source :text)
                                                     :start start))))
                (t (error (make-condition 'lex-error
                                          :text (getf source :text)
                                          :location (getf source :location)))))))))


(defun look-ahead (source)
  (let ((saved-location (getf source :location)))
    (multiple-value-prog1 (next-token source)
      (setf (getf source :location) saved-location))))


(declaim (ftype (function (list) t) expression))

(defun term (source)
  (multiple-value-bind (token text) (next-token source)
    (case token
      (:notsym (list :not (term source)))
      (:labelsym text)
      (:lparensym (prog1 (expression source)
                    (when (not (eq (look-ahead source) :rparensym))
                      (error (make-condition 'parsing-error
                                             :text (getf source :text)
                                             :location (getf source :location)
                                             :error-message "Missing ')'.")))
                    (next-token source)))
      (nil (error (make-condition 'parsing-error
                                  :text (getf source :text)
                                  :location (getf source :location)
                                  :error-message "Unexpected end of input.")))
      (otherwise (error (make-condition 'parsing-error
                                        :text (getf source :text)
                                        :location (getf source :location)
                                        :error-message "Unexpected symbol."))))))


(defun conjunction (source)
  (let ((left (term source)))
    (if (eq (look-ahead source) :andsym)
        (progn (next-token source)
               (let ((right (conjunction source)))
                 (if (and (consp right) (eq :and (car right)))
                     (list* :and left (cdr right))
                     (list :and left right))))
        left)))


(defun disjunction (source)
  (let ((left (conjunction source)))
    (if (eq (look-ahead source) :orsym)
        (progn (next-token source)
               (let ((right (disjunction source)))
                 (if (and (consp right) (eq :or (car right)))
                     (list* :or left (cdr right))
                     (list :or left right))))
        left)))


(defun expression (source)
  (disjunction source))


(defun compile-expression (expression)
  "Compile the text expression EXPRESSION so that it can be matched
against label sets."
  (let ((source (list :text expression :location 0)))
    (prog1 (expression source)
      (when (look-ahead source)
        (error (make-condition 'parsing-error
                               :error-message "End of input expected. Maybe missing && or ||?"
                               :text expression
                               :location (getf source :location)))))))


(defun negation-normal-form (form)
  (labels ((distribute-negation (form)
             (if (eq :not (car form))
                 (cadr form)
                 (cons (if (eq :and (car form)) :or :and)
                       (mapcar (curry #'list :not) (cdr form))))))
    (cond ((and (consp form) (eq :not (car form)))
           (if (consp (cadr form))
               (negation-normal-form (distribute-negation (cadr form)))
               form))
          ((consp form)
           (cons (car form) (mapcar #'negation-normal-form (cdr form))))
          (t form))))


(defun flatten-form (form)
  (let ((subtree (if (consp form)
                     (cons (car form)
                           (mapcar #'flatten-form (cdr form)))
                     form)))
    (if (and (consp form) (member (car form) '(:and :or)))
        (mapcan #'(lambda (x)
                    (if (and (consp x) (eq (car form) (car x)))
                        (copy-seq (cdr x))
                        (list x)))
                subtree)
        subtree)))


(defun distribute-or (form)
  (cond ((and (consp form) (eq :or (car form)))
         (let ((sorted (sort (copy-seq (cdr form))
                             #'(lambda (x y)
                                 (declare (ignore y))
                                 (and (consp x) (eq (car x) :and))))))
           (if (and (consp (car sorted)) (eq :and (caar sorted)))
               (distribute-or (cons :and
                                    (mapcar #'(lambda (x)
                                                (list* :or x (cdr sorted)))
                                            (cdar sorted))))
               form)))
        ((consp form) (cons (car form) (mapcar #'distribute-or (cdr form))))
        (t form)))


(defun conjunctive-normal-form (form)
  "Reduce compiled label expression FORM into CNF"
  (flatten-form (distribute-or (flatten-form (negation-normal-form form)))))


;; NOTE: This function assumes both P and Q are disjunctions of literals.
(defun -> (p q)
  (or (equal p q)
      (and (consp p)
           (consp q)
           (>= (length q) (length p))
           (every (rcurry #'member q :test #'equal) p))
      (and (consp q)
           (member p q :test #'equal))))


(defun literalp (p)
  (or (and (consp p) (eq (car p) :not) (stringp (cadr p)))
      (stringp p)))


;; NOTE: This function assumes P and Q are both literals or negative literals.
(defun literal-lessp (p q)
  (cond ((and (consp p) (consp q)) (string-lessp (cadr p) (cadr q)))
        ((consp p))
        ((and (stringp q) (string-lessp p q)))))


;; NOTE: This function assumes P and Q are both in CNF.
(defun clause-lessp (p q)
  (cond ((and (literalp p) (literalp q)) (literal-lessp p q))
        ((literalp p))
        ((not (literalp q))
         (or (> (length q) (length p))
             (let ((idx (mismatch p q :test #'equal)))
               (if idx (literal-lessp (elt p idx) (elt q idx))))))))


(defun minimally-canonical-conjunctive-normal-form (form)
  "Reduce compiled label expression FORM into minimally canonical CNF.
Minimally canonical CNF is CNF with the following further
restrictions:

  1) No disjunction in the conjunction is implied by any other
     disjunction in the conjunction.
  2) The literals in each disjunction are ordered according to a
     canonical literal ordering.
  3) The disjunctions are ordered according to a canonical disjunction
     ordering."
  (if (consp form)
      (let* ((form (conjunctive-normal-form form))
             (deduped (remove-duplicates (remove-if #'(lambda (p)
                                                        (some #'(lambda (q)
                                                                  (and (-> q p)
                                                                       (not (-> p q))))
                                                              form))
                                                    form)
                                         :test #'->)))
        (cons :and
              (sort (mapcar #'(lambda (sub-form)
                                (if (and (consp sub-form) (eq :or (car sub-form)))
                                    (cons :or
                                          (sort (copy-seq (cdr sub-form))
                                                #'clause-lessp))
                                    sub-form))
                            (cdr deduped))
                    #'clause-lessp)))
      form))
