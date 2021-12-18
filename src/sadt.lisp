(defpackage :sadt
  (:use :cl :cl-user)
  (:export :defadt :make-adt :match))

(in-package :sadt)

;;; taken from alexandria
(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (reduce #'+ things
                         :key (lambda (x) (length (string x)))))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
               (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))

(defun gen-clauses (name clauses)
  (mapcar
   #'(lambda (clause)
       (destructuring-bind (clause-name &rest vars) clause
         `(defclass ,(symbolicate name "-" clause-name) (,name)
            (,@(mapcar #'(lambda (var-name)
                           `(,var-name :initarg ,(intern (string-upcase
                                                          (symbol-name var-name))
                                                         "KEYWORD")
                                       :accessor ,var-name))
                       vars)))))
   clauses))

(defmacro defadt (name &body clauses)
  `(progn
     (defclass ,name () ())
     ,@(gen-clauses name clauses)))

(defmacro make-adt (name tag &body arg-plist)
  `(make-instance ',(symbolicate name "-" (symbol-name tag))
                 ,@arg-plist))

(defmacro match (name var &body clauses)
  (let ((v (gensym)))
    `(let ((,v ,var))
       (progn
         (cond
           ,@(mapcar #'(lambda (clause)
                         (destructuring-bind ((cname &rest vars) &rest body) clause
                           `((typep ,v ',(symbolicate name "-" cname))
                             (let (,@(mapcar #'(lambda (vari)
                                                 (cond
                                                   ((listp vari)
                                                    (list (cadr vari) (list (car vari) v)))
                                                   ((symbolp vari)
                                                    `(,vari (,vari ,var)))))
                                             vars))
                               ,@body))))
                     clauses))))))
