#+nil
(progn
  (ql:quickload :uncommon-lisp)
  (ql:quickload :utility))

(defpackage #:chunk
  (:use :cl))
(in-package :chunk)

(utility:eval-always
  (defparameter *chunk-size-x* 16)
  (defparameter *chunk-size-y* 16)
  (defparameter *chunk-size-z* 16))

(struct-to-clos:struct->class
 (defstruct chunk
   x
   y
   z
   data))

(deftype chunk-coord () 'fixnum)
(deftype chunk-data () `(simple-array t (,*chunk-size-x* ,*chunk-size-y* ,*chunk-size-z*)))
(deftype inner-chunk-coord-x () `(integer 0 ,*chunk-size-x*))
(deftype inner-chunk-coord-y () `(integer 0 ,*chunk-size-y*))
(deftype inner-chunk-coord-z () `(integer 0 ,*chunk-size-z*))
(deftype block-coord () 'fixnum)
(defun create-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
  (declare (type chunk-coord chunk-x chunk-y chunk-z))
  (make-chunk :x chunk-x
	      :y chunk-y
	      :z chunk-z
	      :data (make-array (list *chunk-size-x* *chunk-size-y* *chunk-size-z*)
				:initial-element nil)))
(utility::with-declaim-inline (obtain-chunk reference-inside-chunk get-chunk
					    get-chunk-from-chunk-array
					    (setf reference-inside-chunk))
  (utility:with-unsafe-speed
    (defun chunk-coordinates-match-p (chunk &optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      (and (= chunk-x (the chunk-coord (chunk-x chunk)))
	   (= chunk-y (the chunk-coord (chunk-y chunk)))
	   (= chunk-z (the chunk-coord (chunk-z chunk)))))

    (defun meh (&optional (x 0) (y 0) (z 0))
      (declare (type block-coord x y z))
      (let ((chunk-x (floor x (utility:etouq *chunk-size-x*)))
	    (chunk-y (floor y (utility:etouq *chunk-size-y*)))
	    (chunk-z (floor z (utility:etouq *chunk-size-z*)))
	    (inner-x (mod x (utility:etouq *chunk-size-x*)))
	    (inner-y (mod y (utility:etouq *chunk-size-y*)))
	    (inner-z (mod z (utility:etouq *chunk-size-z*))))
	(let ((chunk (obtain-chunk chunk-x chunk-y chunk-z)))
	  (reference-inside-chunk chunk inner-x inner-y inner-z))))
    (defun (setf meh) (value &optional (x 0) (y 0) (z 0))
      (declare (type block-coord x y z))
      (let ((chunk-x (floor x (utility:etouq *chunk-size-x*)))
	    (chunk-y (floor y (utility:etouq *chunk-size-y*)))
	    (chunk-z (floor z (utility:etouq *chunk-size-z*)))
	    (inner-x (mod x (utility:etouq *chunk-size-x*)))
	    (inner-y (mod y (utility:etouq *chunk-size-y*)))
	    (inner-z (mod z (utility:etouq *chunk-size-z*))))
	(let ((chunk (obtain-chunk chunk-x chunk-y chunk-z)))
	  (setf (reference-inside-chunk chunk inner-x inner-y inner-z) value))))

    (defun reference-inside-chunk (chunk inner-x inner-y inner-z)
      (declare (type inner-chunk-coord-x inner-x)
	       (type inner-chunk-coord-y inner-y)
	       (type inner-chunk-coord-z inner-z))
      (aref (the chunk-data (chunk-data chunk)) inner-x inner-y inner-z))
    (defun (setf reference-inside-chunk) (value chunk inner-x inner-y inner-z)
      (declare (type inner-chunk-coord-x inner-x)
	       (type inner-chunk-coord-y inner-y)
	       (type inner-chunk-coord-z inner-z))
      (setf (aref (the chunk-data (chunk-data chunk)) inner-x inner-y inner-z) value))

    (defun obtain-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      (or (get-chunk-from-chunk-array chunk-x chunk-y chunk-z)
	  (get-chunk chunk-x chunk-y chunk-z))))

  ;;equal is used because the key is a list of the chunk coordinates
  (defparameter *chunks* (make-hash-table :test 'equal))
  (utility:with-unsafe-speed
    (defun get-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
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
		  (values new-chunk t))))))))

  (defun load-chunk (&optional (chunk-x 0) (chunk-y 0) (chunk-z 0))
    ;;FIXME::actually load chunks
    (create-chunk chunk-x chunk-y chunk-z))

  (utility::eval-always
    (defparameter *chunk-array-default-size-x* 32)
    (defparameter *chunk-array-default-size-y* 32)
    (defparameter *chunk-array-default-size-z* 32))
  (struct-to-clos:struct->class
   (defstruct chunk-array
     (array (make-array (list *chunk-array-default-size-x*
			      *chunk-array-default-size-y*
			      *chunk-array-default-size-z*)
			:initial-element nil))
     (x-min 0)
     (y-min 0)
     (z-min 0)
     (x-max *chunk-array-default-size-x*)
     (y-max *chunk-array-default-size-y*)
     (z-max *chunk-array-default-size-z*)))

  (defun create-chunk-array ()
    (make-chunk-array))
  (defun reposition-chunk-array (&optional 
				   (chunk-x 0) (chunk-y 0) (chunk-z 0)
				   (chunk-array *chunk-array*))
    (setf (chunk-array-x-min chunk-array) chunk-x
	  (chunk-array-y-min chunk-array) chunk-y
	  (chunk-array-z-min chunk-array) chunk-z)
    (setf (chunk-array-x-max chunk-array) (+ (chunk-array-size-x chunk-array) chunk-x)
	  (chunk-array-y-max chunk-array) (+ (chunk-array-size-y chunk-array) chunk-y)
	  (chunk-array-z-max chunk-array) (+ (chunk-array-size-z chunk-array) chunk-z))
    (values))

  (defparameter *chunk-array* (create-chunk-array))
  (deftype chunk-array-data ()
    `(simple-array t (,*chunk-array-default-size-x*
		      ,*chunk-array-default-size-y*
		      ,*chunk-array-default-size-z*)))

  (utility:with-unsafe-speed
    (defun get-chunk-from-chunk-array (&optional 
					 (chunk-x 0) (chunk-y 0) (chunk-z 0)
					 (chunk-array *chunk-array*))
      (declare (type chunk-coord chunk-x chunk-y chunk-z))
      ;;if the coordinates are correct, return a chunk, otherwise return nil
      (let ((data-x (- chunk-x (the chunk-coord (chunk-array-x-min chunk-array)))))
	(declare (type chunk-coord data-x))
	(when (< -1 data-x ;;(the chunk-coord (chunk-array-size-x chunk-array))
		 (utility:etouq *chunk-array-default-size-x*))
	  (let ((data-y (- chunk-y (the chunk-coord (chunk-array-y-min chunk-array)))))
	    (declare (type chunk-coord data-y))
	    (when (< -1 data-y ;;(the chunk-coord (chunk-array-size-y chunk-array))
		     (utility:etouq *chunk-array-default-size-y*))
	      (let ((data-z (- chunk-z (the chunk-coord (chunk-array-z-min chunk-array)))))
		(declare (type chunk-coord data-z))
		(when (< -1 data-z ;;(the chunk-coord (chunk-array-size-z chunk-array))
			 (utility:etouq *chunk-array-default-size-z*))
		  (let ((data (chunk-array-array chunk-array)))
		    (declare (type chunk-array-data data))
		    ;;the chunk is in the chunk-array's bounds
		    (let ((possible-chunk
			   (aref data data-x data-y data-z)))
		      (if (and possible-chunk
			       (chunk-coordinates-match-p possible-chunk chunk-x chunk-y chunk-z))
			  ;;The chunk is not nil, and the coordinates line up
			  possible-chunk
			  (let ((next-possible-chunk (get-chunk chunk-x chunk-y chunk-z)))
			    (setf (aref data data-x data-y data-z) next-possible-chunk)
			    next-possible-chunk))))))))))))
  )
