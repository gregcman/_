(defpackage #:terminal625
  (:use #:cl))

(in-package :terminal625)
(defparameter *self-path*
  (let ((path #.(or *compile-file-truename*
		    *load-truename*)))
    (make-pathname :host (pathname-host path)
		   :directory (pathname-directory path))))

(defun map-package (fun &optional (package *package*))
  (let ((package (find-package package)))
    (do-symbols (sym package)
      (when (eq package
		(symbol-package sym))
	(funcall fun sym)))))

(defun vars (&optional (fun #'print) (package *package*))
  (map-package (lambda (x)
		 (when (boundp x)
		   (funcall fun x)))
	       package))

(defun funs (&optional (fun #'print) (package *package*))
  (map-package (lambda (x)
		 (when (fboundp x)
		   (funcall fun x)))
	       package))

(defun repl (&optional (end :end))
  (catch end
    (loop
       (progn
;	 (princ (package-name *package*))
;	 (write-char #\>)
;	 (write-char #\Space)
	 )
       (force-output)
       (let* ((form (read))
	      (results
	       (multiple-value-list
		(eval form))))
	 (setf - form)
	 (shiftf /// // / results)
	 (shiftf +++ ++ + -)
	 (shiftf *** ** * (first results))

	 (dolist (item results)
	   (print item))

	 (terpri)))))

(defmacro etouq (form)
  (let ((var (gensym)))
    `(macrolet ((,var () ,form))
       (,var))))


