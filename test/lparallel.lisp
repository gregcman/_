(defpackage #:lp
  (:use :cl))
(in-package #:lp)

(defun quickload ()
  (progn
    (ql:quickload :lparallel)
    (ql:quickload :cl-cpus)))

(defparameter *channel* nil)
(defun reset ()
  (lparallel::end-kernel)
  (setf lparallel::*kernel* (lparallel::make-kernel (cpus:get-number-of-processors)))
  (setf *channel* (lparallel:make-channel)))

(defparameter *print-errors* t)
(defmacro debugging (&body body)
  `(when *print-errors*
     ,@body))

(defun submit (fun &rest args)
  (lparallel:task-handler-bind
      ((error #'lparallel:invoke-transfer-error))
      (lparallel:submit-task
       *channel*
       (lambda ()
	 (apply fun args)
	 #+nil
	 (handler-case 
	     
	     (error (c)
		    (declare (ignorable c))
		    (debugging
		      (print c))))))))

(defun get-values ()
  (loop
     (handler-case 
	 (multiple-value-bind (value exist-p) (lparallel:try-receive-result *channel*)
	   (unless exist-p
	     (return))
	   (print value))
       (error (c)
	 (declare (ignorable c))
	 (debugging (print c))))))

(defun test ()
  (dotimes (x 1)
    (submit (lambda () (error "fuck you")))))
