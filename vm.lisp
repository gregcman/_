(in-package :terminal625)

#+nil
(
;;;can jump to any point in the function
;;;each evaluation can be interrupted and resumed
 (defparameter *arr* (make-array 128))


 (defun quadratic-formula (a b c)
   (let ((det (sqrt (- (* b b)
		       (* 4 a c))))
	 (denominator (* 2 a))
	 (minus-b (- b)))
     (values
      (/ (+ minus-b det) denominator)
      (/ (- minus-b det) denominator)))))

;;;(x - 6) * (7x - 7) = 0
;;;7x^2 - 7x + 42 - 42x
;;;7x^2 - 49x + 42
#+nil
(let*
    let

  quote
  eval-when
  load-time-value
  symbol-macrolet
  locally
  macrolet
  the

  tagbody
  go

  flet
  labels

  block
  return-from

  catch
  throw

  unwind-protect

  setq

  function
  multiple-value-call
  multiple-value-prog1

  if
  progn)

;;let let* if progn setq catch throw unwind-protect block return-from go tagbody quote

;;multiple values?
;;macros?
;;closures??

(defparameter *defs* 
  (quote
   ((define (drive form)
      (cond ((atom form)
	     (oureval form (quote ()) (getproc)))
	    ((eq (car form) (quote define))
	     (setproc 
	      (bind (list (caadr form))
		    (list (list (cdadr form) (caddr form)))
		    (getproc))))
	    (t (oureval form (quote ()) (getproc)))))
    (define (oureval exp env procedures)
      (cond ((atom exp)
	     (cond ((eq exp (quote nil)) (quote nil))
		   ((eq exp (quote t)) (quote t))
		   ((numberp exp) exp)
		   ((stringp exp) exp)
		   (t (value exp env))))
	    ((eq (car exp) (quote quote))
	     (cadr exp))
	    ((eq (car exp) (quote cond))
	     (evcond (cdr exp) env procedures))
	    (t (ourapply (value (car exp) procedures)
			 (evlis (cdr exp) env procedures)
			 procedures))))
    (define (ourapply fun args procedures)
      (cond ((functionp fun) (apply fun args))
	    (t (oureval (cadr fun)
			(bind (car fun) args (quote ()))
			procedures))))
    (define (evcond clauses env procedures)
      (cond ((null clauses) (error "no clauses in cond"))
	    ((oureval (caar clauses) env procedures)
	     (oureval (cadar clauses) env procedures))
	    (t (evcond (cdr clauses) env procedures))))
    (define (evlis arglist env procedures)
      (cond ((null arglist) (quote ()))
	    (t (cons (oureval (car arglist) env procedures)
		     (evlis (cdr arglist) env procedures)))))
    (define (bind vars args env)
      (cond ((= (length vars) (length args))
	     (cons (cons vars args) env))
	    (t (error "argument mismatch: ~s ~s" vars args))))
    (define (value name env)
      (value1 name (lookup name env)))
    (define (value1 name slot)
      (cond ((eq slot (quote &unbound))
	     (error "slot unbound: ~a" name))
	    (t (car slot))))
    (define (lookup name env)
      (cond ((null env) (quote &unbound))
	    (t (lookup1 name (caar env) (cdar env) env))))
    (define (lookup1 name vars vals env)
      (cond ((null vars) (lookup name (cdr env)))
	    ((eq name (car vars)) vals)
	    (t (lookup1 name (cdr vars) (cdr vals) env))))
    )))

(defun getproc () *procedures*)
(defun setproc (x) (setf *procedures* x))

(progn
  (defparameter *procedures* nil)
  (defparameter *funs*
    (quote (CAADR
	    LIST
	    CDADR CADDR
	    ATOM NUMBERP
	    EXP FUNCTIONP
	    APPLY CADR
	    CADAR =
	    LENGTH CONS
	    ERROR QUOTE
	    CAAR CDAR
	    NULL
	    EQ CAR
	    CDR
	    stringp
	    setproc
	    getproc

	    *
	    -
	    +
	    =
	    /)))
  (setf *procedures*
	(bind *funs* (mapcar 'symbol-function *funs*)
	      *procedures*))
  (map nil (function drive) *defs*)
  (etouq (cons 'progn *defs*)))

(defmacro define ((name &rest args) &body body)
  `(defun ,name ,args . ,body))


(defparameter *test* '(+ 2 (* 34 34) (+ 2 2 (* 7 74))))
(defun drivequote (x form)
  (if (= x 0)
      form
      (drivequote (1- x) `(drive (quote ,form)))
      ))

(defun test (nests form times)
  (let ((dayum (drivequote nests form)))
    (setf times (expt 10 times))
    (eval (print
	   `(time (let ((a nil))
		    (dotimes (x ,times)
		      (setf a ,dayum))
		    a))))))
