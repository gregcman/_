(ql:quickload :shadow)
(ql:quickload :varjo)
(ql:quickload :utility)

(defpackage :variables
  (:use :cl))
(in-package :variables)
#+nil
(defparameter *keyword-package* (find-package "KEYWORD"))

(defparameter *bar*
  ;nil
  ;#+nil
  '((values (vec4 b b b b)
     (swizzle (vec4 b b b b)
      :rgba))))

(defpackage :varjo-shader
  (:use :vari :cl;; :shadow
	))

(defun intern-for-varjo (&optional (tree *bar*)
			   (package (find-package "VARJO-SHADER")))
  (let ((packages-which-use-vari (package-used-by-list :vari))
	;(shadow-package (find-package "SHADOW"))
	)
    (let (stack)
      (block out
	(labels ((rec (tree)
		   (mapcar
		    (lambda (x)
		      (typecase x
			(list (if (member x stack)
				  (error "error! This is a circular Tree!")
				  (unwind-protect
				       (progn (push x stack)
					      (rec x))
				    (pop stack))))
			(symbol
			 (if (or (keywordp x)
				 (let ((symbol-package (symbol-package x)))
				   ;;(or)
				   (find
				    symbol-package
				    packages-which-use-vari)
				   #+nil
				   (eq symbol-package
				       shadow-package)))
			     x
			     (intern (symbol-name x)
				     package)))
			(otherwise x)))
		    tree)))
	  (rec tree))))))

(defmacro with-varjo-shader-package (&body body)
  `(let ((*package* (find-package "VARJO-SHADER")))
     ,@(intern-for-varjo body)))
(defparameter varjo-shader::*glsl-version* :140)

(defmacro varjo-shader::define-program (name (&optional (primitive :triangles)) &body body)
  `(shadow:define-shader ,name (:version varjo-shader::*glsl-version* :primitive ,primitive)
     ,@body))

(defparameter *foo*
  (let ((version '(:140)))
    (list
     (varjo:make-stage :vertex (variables::intern-for-varjo '((b :float "FOOBAR")))
		       nil
		       version
		       (variables::intern-for-varjo variables::*bar*))
     (varjo:make-stage :fragment (variables::intern-for-varjo '((g :vec4)))
		       nil
		       version
		       (variables::intern-for-varjo '(g))))))

(defun )
(let ((uniforms-var (gensym)))
    `(let ((,uniforms-var (gl-program-object-uniforms ,program-object)))
       (macrolet ((,name (id)
		    (list 'getuniform ',uniforms-var id)))
	 ,@body)))
(defmacro with-uniforms (name program-object &body body)
  (utility:with-gensyms (uniform program uniforms)
    `(let* ((,program ,program-object)
	    (,uniforms (shadow::uniforms ,program)))
       (flet ((,name (,uniform)
		(au:href ,uniforms ,uniform :location)))
	 ,@body))))

(shadow:define-function)

(defpackage :random)
(in-package :random)
(variables::with-varjo-shader-package
  (define-function foo-vert ((position :vec3) (uv :vec2) &uniform (mvp :mat4))
    (let ((wow (vec3 4)))
      (values (* mvp (vec4 position 1))
	      (vec4 1 0 0 1))))
  (define-function foo-frag ((color :vec4))
    (values color))
  (define-function foo-frag2 ((color :vec4))
    (values color))
  (define-program example-program (:points)
    (:vertex (foo-vert :vec3 :vec2))
    (:fragment (foo-frag :vec4))))

(defpackage :varjo-compile
  (:use :varjo :cl))
(in-package :varjo-compile)

(defun dump-glsl ()
  (let ((obj (varjo:rolling-translate
	      variables::*foo*)))
    (values obj
	    (varjo:glsl-code
	     obj))))
