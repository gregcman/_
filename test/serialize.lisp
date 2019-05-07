(in-package :cl-user)
(defun noop (&optional data)
  (sandbox.serialize::decode-zlib-conspack-payload
   (sandbox.serialize::encode-zlib-conspack-payload data)))
(progn
  (defparameter *data1* (make-array (* 16 16 16)))
  (map-into *data1* (lambda ()
		      ;;(world::blockify (random 256) (random 16) (random 16))
		      ;;(world::blockify 0 0 (random 16))
		      (random 16)
		      ;;most-positive-fixnum
		      )))
(progn
  (defparameter *keywords*
    '(:grass :dirt :stone :void :cobble progn bar foo foobar bazquxasdfasfasdf
      safjasdfasdf))
  (defparameter *data2* (make-array (* 16 16 16)))
  (map-into *data2* (lambda ()
		      (elt *keywords* (random (length *keywords*))))))
(progn
  (defparameter *data3* (make-array (* 16 16 16)))
  (map-into *data3* (lambda ()
		      (create-light (random 16)))))

(defun test34 ()
  (let ((times (expt 10 3)))
    (time
     (dotimes (i times)
       (noop *data1*)))
    (time
     (dotimes (i times)
       (noop *data2*)))
    (time
     (dotimes (i times)
       (noop *data3*)))
    (values)))

(defun test43 ()
  (list (length (sandbox.serialize::encode-zlib-conspack-payload *data1*))
	(length (sandbox.serialize::encode-zlib-conspack-payload *data2*))
	(length (sandbox.serialize::encode-zlib-conspack-payload *data3*))))

(defparameter *full-optimize* '(optimize (speed 3) (safety 0)))
(defparameter *cases*
  '((:light0 0)
    (:light1 1)
    (:light2 2)
    (:light3 3)
    (:light4 4)
    (:light5 5)
    (:light6 6)
    (:light7 7)
    (:light8 8)
    (:light9 9)
    (:light10 10)
    (:light11 11)
    (:light12 12)
    (:light13 13)
    (:light14 14)
    (:light15 15)))

(defparameter *keyword-array* (make-array (length *cases*) :initial-contents (mapcar 'first *cases*)))
(declaim (notinline light->num light-value light-value2 light-value3 numlight))
(defun light->num (light)
  (declare #.*full-optimize*)
  (utility:etouq
    `(case light
       ,@*cases*)))
(defun num->light (num)
  (declare #.*full-optimize*)
  (utility::etouq `(svref ,*keyword-array* num)))

(defun numlight (lightnum)
  (declare #.*full-optimize*
	   (type fixnum lightnum))
  (ldb (byte 4 8) lightnum))

(defstruct light
  (value 0 :type (unsigned-byte 4)))

(defun lightlight (lightnum)
  (declare #.*full-optimize*
	   (type light lightnum))
  (light-value lightnum))

(defun create-light (n)
  (aref *light-table* n))

(progn
  (defparameter *light-table* (make-array 16 :element-type 'light))
  (dotimes (i (length *light-table*))
    (setf (aref *light-table* i) (make-light :value i))))

(defmethod make-load-form ((self light) &optional environment)
  (declare (ignore environment))
  `(create-light ,(light-value self)))

(defun light-value2 (light)
  (declare #.*full-optimize*
	   (type light light))
  (utility:etouq
    `(cond ,@(loop :for n :below 16 :collect `((eq light ,(create-light n)) ,n)))))

(defun light-value3 (light)
  (declare #.*full-optimize*
	   (type light light))
  (light-value light))

(defmethod conspack:encode-object ((object light) &key &allow-other-keys)
  `((,(light-value object) . nil)))

(defmethod conspack:decode-object ((class (eql 'light)) alist
                                   &key &allow-other-keys)
  (create-light (car (car alist))))

(defun test67 ()
  (let ((times (expt 10 7))
	(value 15
	  ))
    (let ((light (num->light value)))
      (time (dotimes (x times)
	      (light->num light))))
    (let ((light (create-light value)))
      ;;why does light-value2 take so much longer than light->num?
      ;;or does it? some sort of illusion?
      ;;premature optimization?
      (time (dotimes (x times)
	      (light-value2 light)))
      (time (dotimes (x times)
	      (light-value3 light)))
      (time (dotimes (x times)
	      (light-value light))))
    (time (dotimes (x times)
	    (numlight value)))))
