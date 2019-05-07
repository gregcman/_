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
   return-values))

(defmethod print-object ((object job-task) stream)
  (format stream "<job-task ~s ~s>" (job-task-status object) (job-task-return-values object)))
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
(defun abort-job-task (job-task)
  (with-locked-job-task (job-task)
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
	(when (eq status :running)
	  ;;kill a task that has been started
	  (bordeaux-threads:destroy-thread (job-task-thread job-task)))
	(setf (job-task-thread job-task) nil)
	;;(lparallel:kill-tasks job-task)
	(setf (job-task-status job-task) :killed)
	(lparallel.queue:push-queue job-task *finished-task-queue*))))
  job-task)

(defun job-task-function (job-task fun args)
  (when (init-job-task job-task)
    (handler-case  
	(progn	  
	  (complete-job-task job-task (multiple-value-list (apply fun args))))
      (error (c)
	;;handle regular errors from function
	(declare (ignorable c))
	(debugging (print c))
	(abort-job-task job-task))))
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
      (loop
	 (multiple-value-bind (value exist-p) (lparallel:try-receive-result *channel*)
	   (unless exist-p
	     (return))
	   (when (typep value 'job-task))
	   (lparallel.queue:push-queue/no-lock value queue))))))

(defun get-values (&optional (fun 'print))
  (%get-values)
  (let ((queue *finished-task-queue*))
    (lparallel.queue:with-locked-queue queue
      (loop
	 (multiple-value-bind (value exist-p) (lparallel.queue:try-pop-queue/no-lock queue)
	   (unless exist-p
	     (return))
	   (funcall fun value))))))

(defun test ()
  (dotimes (x 10)
    (submit (lambda () (error "fuck you")))))
(defun test-loop ()
  (let ((task (submit (lambda () (loop)))))
    (sleep 1)
    (kill-job-task task)))
