(in-package :sadt)

(defadt list-adt
  (nil)
  (cons hd tl))

(defvar *x* (make-adt list-adt nil))

(defvar *y* (make-adt list-adt cons :hd "head!" :tl "tail!"))

(defun show (thing)
  (match list-adt thing
    ((nil) (list :its-a-nil))
    ((cons hd tl) (list hd tl :its-a-cons))))
