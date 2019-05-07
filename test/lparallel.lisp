(defpackage #:lp
  (:use :cl))
(in-package #:lp)

(defun quickload ()
  (progn
    (ql:quickload :lparallel)
    (ql:quickload :cl-cpus)
    (ql:quickload :uncommon-lisp)))

(defparameter *channel* nil)
(defparameter *finished-task-queue* nil)
(defun reset ()
  (lparallel::end-kernel)
  (setf lparallel::*kernel* (lparallel::make-kernel (cpus:get-number-of-processors)))
  (setf *channel* (lparallel:make-channel))
  (setf *finished-task-queue* (lparallel.queue:make-queue)))

(defparameter *print-errors* t)
(defmacro debugging (&body body)
  `(when *print-errors*
     ,@body))

(struct-to-clos:struct->class 
 (defstruct job-task
   thread
   status
   ;;status is one of :pending, :started, :complete, :aborted, :killed
   ;;:pending -> created, not run yet
   ;;:running = in the process of running
   ;;:aborted = aborted from the running function
   ;;:killed = killed from a separate thread
   ;;:completed = finished normally
   (lock (bordeaux-threads:make-recursive-lock))
   return-values
   (return-status t)
   ;;return-status is an error object when status is :aborted,
   ;;when status is :killed, it is nil
   ;;otherwise it is t
   ))

(defmethod print-object ((object job-task) stream)
  (format stream "<job-task ~s ~s ~s>"
	  (job-task-status object)
	  (job-task-return-values object)
	  (job-task-return-status object)))
(defun init-job-task (job-task)
  ;;return t if correctly initialized
  ;;otherwise return nil. Should return nil if the task was killed beforehand
  (with-locked-job-task (job-task)
    (let ((old-status (job-task-status job-task)))
      (cond ((eq old-status :pending)
	     (setf (job-task-status job-task) :running)
	     (setf (job-task-thread job-task) (bordeaux-threads:current-thread))
	     t)
	    (t nil)))))
(defun complete-job-task (job-task returned-values)
  (with-locked-job-task (job-task)
    (setf (job-task-return-values job-task) returned-values)
    (setf (job-task-status job-task) :complete)
    (setf (job-task-thread job-task) nil)))
(defun abort-job-task (job-task error)
  (with-locked-job-task (job-task)
    (setf (job-task-return-status job-task) error)
    (setf (job-task-status job-task) :aborted)
    (setf (job-task-thread job-task) nil)))

(defmacro with-locked-job-task ((job-task) &body body)
  `(bordeaux-threads:with-recursive-lock-held ((job-task-lock ,job-task))
     ,@body))
(defun kill-job-task (job-task)
  (with-locked-job-task (job-task)
    (let ((status (job-task-status job-task)))
      (when
	  ;;only kill job-tasks that are in the middle of processing, or pending
	  (member status '(:pending :running))
	(let (;;set-job-task-vars removes the thread from the object, so save it to 'thread
	      (thread (job-task-thread job-task)))
	  (setf (job-task-thread job-task) nil)
	  (setf (job-task-status job-task) :killed)
	  (setf (job-task-return-status job-task) nil)
	  ;;FIXME::use bordeaux threads and kill the thread directly or use lparallel:kill-tasks?
	  ;;(lparallel:kill-tasks job-task)
	  (when (eq status :running)
	    ;;kill a task that has been started
	    (bordeaux-threads:destroy-thread thread)))
	;;we push to the *finished-task-queue*, because otherwise lparallel does not
	;;let us know about killed task objects
	;;FIXME::This means tasks killed with kill-tasks or bordeaux-threads:destroy-thread
	;;will not be registered correctly, the job-task object will still say :pending/:running
	;;and contain the dead thread.
	(lparallel.queue:push-queue/no-lock job-task *finished-task-queue*))))
  job-task)

(defun job-task-function (job-task fun args)
  (when (init-job-task job-task)
    (handler-case  
	(progn	  
	  (complete-job-task job-task (multiple-value-list (apply fun args))))
      (error (c)
	;;handle regular errors from function
	(declare (ignorable c))
	;;(debugging (print c))
	(abort-job-task job-task c))))
  job-task)
(defun submit (fun &rest args)
  (let ((new-job-task (make-job-task :status :pending)))
    (let ((lparallel:*task-category* new-job-task))
      (lparallel:submit-task
       *channel*
       'job-task-function
       new-job-task
       fun
       args))
    new-job-task))

(defun test23 ()
  (restart-case
      (handler-bind ((error #'(lambda (c)
				(declare (ignore c))
				(invoke-restart 'my-restart 7))))
	(error "Foo."))
    (my-restart (&optional v) v)))

(defun %get-values ()
  ;;receives values from the *channel* which can be either task objects
  ;;or processing errors. Filter out the processing errors and send the
  ;;tasks to the *finished-task-queue*
  (let ((queue *finished-task-queue*))
    (lparallel.queue:with-locked-queue queue
      (loop :named outer-loop :do
	 (handler-case 
	     (loop
		(multiple-value-bind (value exist-p)
		    (lparallel:try-receive-result *channel*)
		  (unless exist-p
		    (return-from outer-loop))
		  (when (typep value 'job-task))
		  (lparallel.queue:push-queue/no-lock value queue)))
	   (error (c)
	     (declare (ignorable c))
	     ;;(print c)
	     ))))))

(defun get-values (&optional (fun 'print))
  (%get-values)
  (let ((queue *finished-task-queue*))
    (lparallel.queue:with-locked-queue queue
      (loop :named loop :do
	 (multiple-value-bind (value exist-p) (lparallel.queue:try-pop-queue/no-lock queue)
	   (unless exist-p
	     (return-from loop))
	   (funcall fun value))))))

(defun test ()
  (dotimes (x 10)
    (submit (lambda () (error "fuck you")))))
(defun test2 ()
  (dotimes (x 10)
    (submit (lambda () (random 100)))))
(defun test-loop ()
  (let ((task (submit (lambda () (loop)))))
    (sleep 0.2)
    (kill-job-task task)))
