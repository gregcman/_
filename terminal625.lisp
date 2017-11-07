(defpackage #:terminal625
  (:use #:cl))

(in-package :terminal625)
(defparameter *self-path*
  (let ((path #.(or *compile-file-truename*
		    *load-truename*)))
    (make-pathname :host (pathname-host path)
		   :directory (pathname-directory path))))
