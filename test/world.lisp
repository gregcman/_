#+nil
(ql:quickload :uncommon-lisp)

(defpackage #:chunk
  (:use :cl))
(in-package :chunk)

(defparameter *chunk-size-x* 16)
(defparameter *chunk-size-y* 16)
(defparameter *chunk-size-z* 16)

(struct-to-clos:struct->class
 (defstruct chunk
   x
   y
   z
   data))

(defun create-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
  (make-chunk :x chunk-x
	      :y chunk-y
	      :z chunk-z
	      :data (make-array (list *chunk-size-x* *chunk-size-y* *chunk-size-z*)
				:initial-element nil)))

(defun meh (&optional (x 0) (y 0) (z 0))
  (let ((chunk-x (floor x *chunk-size-x*))
	(chunk-y (floor y *chunk-size-y*))
	(chunk-z (floor z *chunk-size-z*))
	(inner-x (mod x *chunk-size-x*))
	(inner-y (mod y *chunk-size-y*))
	(inner-z (mod z *chunk-size-z*)))
    (let ((chunk (get-chunk chunk-x chunk-y chunk-z)))
      (reference-inside-chunk chunk inner-x inner-y inner-z))))

(defun reference-inside-chunk (chunk inner-x inner-y inner-z)
  (aref (chunk-data chunk) inner-x inner-y inner-z))

;;equal is used because the key is a list of the chunk coordinates
(defparameter *chunks* (make-hash-table :test 'equal))
(defun get-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
  (let ((key (list chunk-x chunk-y chunk-z)))
    (multiple-value-bind (value existsp) (gethash key *chunks*)
      (if existsp
	  (values value t)
	  (progn
	    ;;FIXME::load chunks here, unload chunks here?
	    #+nil
	    (values nil nil)
	    (let ((new-chunk (load-chunk chunk-x chunk-y chunk-z)))
	      (setf (gethash key *chunks*) new-chunk)
	      (values new-chunk t)))))))

(defun load-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
  ;;FIXME::actually load chunks
  (create-chunk chunk-x chunk-y chunk-z))
