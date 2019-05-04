(in-package :cells)

;;https://github.com/stefano/cells-doc/blob/master/cells-doc.txt
(defmodel hello-cells ()
  ((num :accessor num :initarg :num :initform (c-in 0))
   (square-num :accessor square-num 
	       :initform (c? (* (num self) (num self))))))

(defun hello ()
  (let ((h (make-instance 'hello-cells)))
    (dolist (n '(10 20 30 40 50 60 60))
      (setf (num h) n)
      (format t "num is ~a and square-num is ~a~%" (num h)
	      (square-num h)))))

(defmodel node (family)
  ((val :initform (c-in nil) :initarg :val)))

(defun math-op-family ()
  (let ((root
         (make-instance
          'node
          :val (c? (apply #'+ (mapcar #'val (kids self))))
          :kids
          (c?
            (the-kids
	     (make-kid 'node :md-name :n5 :val (c-in 5))
	     (make-kid
              'node
	      :val (c? (apply #'* (mapcar #'val (kids self))))
	      :kids
	      (c?
		(the-kids
		 (make-kid 'node :md-name :n7 :val 
			   (c-in 7))
		 (make-kid 'node :md-name :n9 :val 
			   (c-in 9))))))))))   
    (format t "value of the tree is ~a~%" (val root))
    (setf (val (fm-other :n7 :starting root)) 10)
    (format t "new value of the tree is ~a~%" (val 
					       root)))) 

(cells-reset)

(defmodel str-model ()
  ((str :accessor str :initform (c-in "") :initarg :str
	:unchanged-if #'equal)
   (rev-str :accessor rev-str :initform (c? (reverse 
					     (str self))))))

(defobserver str ()
  (format t "changed!~%"))

(defun try-str-model ()
  (let ((s (make-instance 'str-model)))
    (dolist (l `("Hello!" "Bye" 
			  ,(concatenate 'string "By" "e") "!olleH"))
      (setf (str s) l)
      (format t "str is \"~a\", rev-str is \"~a\"~%" 
	      (str s) (rev-str s)))))


(defmodel lazy-test ()
  ((lazy-1 :accessor lazy-1 :initform (c-formula (:lazy :once-asked)
                                        (append (val 
						 self)
						(list '!!))))
   (lazy-2 :accessor lazy-2 :initform (c_? (val self)))
   (lazy-3 :accessor lazy-3 :initform (c?_ (reverse 
					    (val self))))
   (val :accessor val :initarg :val :initform (c-in nil))))

(defobserver lazy-1 ()
  (format t "evaluating lazy-1!~%"))

(defobserver lazy-2 ()
  (format t "evaluating lazy-2!~%"))

(defobserver lazy-3 ()
  (format t "evaluating lazy-3!~%"))

(defun print-lazies (l)
  (format t "Printing all the values:~%")
  (format t "lazy-3: ~a~%" (lazy-3 l))
  (format t "lazy-2: ~a~%" (lazy-2 l))
  (format t "lazy-1: ~a~%" (lazy-1 l)))

(defun try-lazies ()
  (let ((l (make-instance 'lazy-test :val (c-in '(Im very lazy!)))))
    (format t "Initialization finished~%")
    (print-lazies l)
    (format t "Changing val~%")
    (setf (val l) '(who will be evaluated?))
    (print-lazies l)))

(defmodel counter ()
  ((how-many :accessor how-many
             :initform (c... (0)
                         (length (^current-elems))))
   (current-elems :accessor current-elems
                  :initform (c-in nil))))

(defun try-counter ()
  (let ((m (make-instance 'counter)))
    (dolist (l '((1 2 3) (4 5) (1 2 3 4)))
      (setf (current-elems m) l)
      (format t "current elements: ~{~a ~}~%" (current-elems m))
      (format t "~a elements seen so far~%" (how-many m)))))

(defmodel cycle ()
  ((cycle-a :accessor cycle-a :initform (c-in nil))
   (cycle-b :accessor cycle-b :initform (c-in nil))))

(defobserver cycle-a ()
  (with-integrity (:change)
    (setf (cycle-b self) new-value)))

(defobserver cycle-b ()
  (with-integrity (:change)
    (setf (cycle-a self) new-value)))

(defun try-cycle ()
  (let ((m (make-instance 'cycle)))
    (setf (cycle-a m) '(? !))
    (format t "~a and ~a" (cycle-a m) (cycle-b m))))

(defmodel syn-time ()
  ((current-time :accessor current-time :initarg :current-time
                 :initform (c-in 0))
   (wait-time :accessor wait-time :initarg :wait-time :initform (c-in 0))
   (time-elapsed :accessor time-elapsed
                 :initform
                 (c?
                   (f-sensitivity :syn ((wait-time self))
                     (current-time self))))))

(defun try-syn-time ()
  (let ((tm (make-instance 'syn-time :wait-time (c-in 2))))
    (dotimes (n 10)
      (format t "time +1~%")
      (incf (current-time tm))
      (format t "time-elapsed is ~a~%" (time-elapsed tm)))))

(defmodel my-syn-test ()
  ((num :accessor num :initform (c-in 0))
   (odd-num :reader odd-num
            :initform (c?
                        (with-synapse :odd-syn ()
                          (if (oddp (^num))
                              (values (^num) :propagate))
			  (values nil :no-propagate))))))

(defobserver odd-num ()
  (when old-value-boundp
    (format t "Propagated!~%")))

(defun try-my-syn ()
  (let ((m (make-instance 'my-syn-test)))
    (dolist (n '(1 2 4 5 7 11 12 14 16 15))
      (format t "Setting num to ~a~%" n)
      (setf (num m) n)
      (format t "odd-num is ~a~%" (odd-num m)))))
