;;; -*- mode: lisp -*-

(defpackage :prog-lang-seminar
  (:use :cl))

(in-package :prog-lang-seminar)

(declaim (optimize safety))


;;; Util

(defmacro do-for ((var start end &optional (step 1)) &body body)
  "Iteratively executes BODY for VAR bound to the fixnums between START and END inclusive, stepping
with STEP.  STEP must be a compile-time constant fixnum."
  (check-type step fixnum)
  (if (<= 0 step)
    `(loop for ,var fixnum from ,start to ,end by ,step
        do (progn ,@body))
    `(loop for ,var fixnum from ,start downto ,end by ,(- step)
        do (progn ,@body))))

(defmacro sum ((var start end) &body body)
  "Iterativle evaluates BODY with VAR bound to the fixnums between START and END, inclusively."
  `(loop for ,var fixnum from ,start to ,end
      sum (progn ,@body)))


;;; Matrixmultiplikation

(defun matmul (A B)
  "Implements matrix multiplication."
  (declare (type (array * 2) A B)
           (optimize (speed 3) (safety 0)))  
  (let* ((n (array-dimension A 0))
         (m (array-dimension A 1))
         (p (array-dimension B 1))
         (C (make-array (list n p) :initial-element 0)))
    (do-for (i 0 (- n 1))
      (do-for (j 0 (- p 1))
        (do-for (k 0 (- m 1))
          (incf (aref C i j)
                (* (aref A i k) (aref B k j))))))
    C))


;;; Gauß Elimination

(defun full-pivoting (A n k)
  "Implements full pivoting strategy."
  (declare (type fixnum n k)
           (type (array * 2) A)
           (optimize (speed 3) (safety 0)))
  (let ((max-i k)
        (max-j k))
    (declare (fixnum max-i max-j))
    (do-for (i k (- n 1))
      (do-for (j k (- n 1))
        (when (< (abs (aref A max-i max-j))
                 (abs (aref A i j)))
          (setf max-i i
                max-j j))))
    (values max-i max-j)))

(define-condition matrix-singular (arithmetic-error)
  ((matrix :initarg :matrix
           :reader  matrix))
  (:report (lambda (condition stream)
             (format stream "The given matrix is singular up to machine precision. ~A"
                     (matrix condition))))
  (:documentation "Error condition for signaling singular matrices."))

(defun decompose-lu (A &key (pivoting #'full-pivoting))
  "Destructively modifies A to return the LU decomposition of A togehter with the row and column
   permutations induced by the pivoting strategy.  The argument PIVOT is a function getting as
   arguments the matrix A, its dimension n and an index k to start searching from."
  (declare (type (array * 2) A)
           (type (function ((array * 2) fixnum fixnum) (values fixnum fixnum))
                 pivoting)
           (optimize (speed 3) (safety 0)))
  (let* ((n   (array-dimension A 0))
         (row (apply #'vector (loop for i fixnum below n collect i)))
         (col (copy-seq row)))
    (do-for (k 0 (- n 1))
      ;; pivoting
      (multiple-value-bind (p_i p_j) (funcall pivoting A n k)
        (declare (fixnum p_i p_j))
        (unless (= k p_i)
          (rotatef (svref row k) (svref row p_i))
          (do-for (i 0 (- n 1))
            (rotatef (aref A p_i i) (aref A k i))))
        (unless (= k p_j)
          (rotatef (svref col k) (svref col p_j))
          (do-for (j 0 (- n 1))
            (rotatef (aref A j p_j) (aref A j k)))))
      ;; check
      (when (zerop (aref A k k))
        (error 'matrix-singular :matrix A))
      ;; actual decomposition
      (do-for (j (+ k 1) (- n 1))
        (let ((s (/ (aref A j k) (aref A k k))))
          (do-for (c (+ k 1) (- n 1))
            (decf (aref A j c)
                  (* s (aref A k c))))
          (setf (aref A j k) s))))
    (values A row col)))

(defun copy-array (A)
  "Does what it says."
  (declare ((array * 2) A))
  (let ((B (make-array (array-dimensions A))))
    (dotimes (i (array-total-size A))
      (setf (row-major-aref B i)
            (row-major-aref A i)))
    B))

(defun gauss (A b &key (pivoting-strategy #'full-pivoting)
                       (copyp nil)
                       (verify nil)
                       (verification-precision 1e-6))
  "Solves the system of linear equations Ax = b by Gauß' algorithm."
  (declare (type (array * 2) A)
           (type (array * 1) b))
  (multiple-value-bind (A row col) (decompose-lu (if copyp (copy-array A) A)
                                                 :pivoting pivoting-strategy)
    (declare (type (array * 2) A)
             (type (simple-array * 1) row col))
    (let* ((n (array-dimension A 0))
           (y (make-array (list n)))
           (x (make-array (list n))))
      (locally
          (declare (optimize (speed 3) (safety 0)))
        ;; forward substitution
        (do-for (i 0 (- n 1))
          (setf (aref y (aref row i))
                (- (aref b (aref row i))
                   (sum (k 0 (- i 1))
                     (* (aref A i k)
                        (aref y (aref row k)))))))
        ;; backward substitution
        (do-for (i (- n 1) 0 -1)
          (setf (aref x (aref col i))
                (/ (- (aref y (aref row i))
                      (sum (k (+ i 1) (- n 1))
                        (* (aref A i k)
                           (aref x (aref col k)))))
                   (aref A i i)))))
      ;; verify
      (when verify
        (let ((computed-rhs (matmul A (make-array (list n 1) :displaced-to x))))
          (unless (loop for i from 0 below n
                     always (<= (abs (- (aref b i) (aref computed-rhs i 0)))
                                verification-precision))
            (error "Gauß Algorithm:  Computed result not correct up to given precision."))))
      ;; return result
      x)))

;;; Newton Verfahren skalar

(defun newton (f x_start &key (epsilon 1e-6))
  (declare (function f)
           (number x_start epsilon)
           (optimize (speed 3) (safety 0)))
  (let ((f-prime (derive f :epsilon epsilon)))
    (declare (function f-prime))
    (labels ((iterate (x_n)
               (let ((x_n+1 (- x_n
                               (/ (funcall f x_n)
                                  (funcall f-prime x_n)))))
                 (if (<= (abs (funcall f x_n+1)) epsilon)
                   x_n+1
                   (iterate x_n+1)))))
      (iterate x_start))))

(defun derive (f &key (epsilon 1e-6))
  (declare (function f)
           (number epsilon)
           (optimize (speed 3) (safety 0)))
  (lambda (x)
    (/ (- (funcall f (+ x epsilon))
          (funcall f (- x epsilon)))
       (* 2 epsilon))))

;;; Testing Utilities

(defun print-random-data (n k)
  "Prints N times a row of N fixnums between 0 and K (exclusively).  If K is fixnum, the
values will be fixnum.  If K is integer, the values will be integer."
  (loop for i from 0 below n
     do (format t "~&")
        (loop for j from 0 below n
           do (format t " ~16,4A" (random k)))))

#|

(with-open-file (*standard-output* "testdata"
                 :direction :output
                 :if-exists :supersede)
  (print-random-data 100 100.0))

|#

(defun read-matrix ()
  "Returns a square matrix as read from *STANDARD-INPUT*."
  (let* ((first-line (read-from-string (concatenate 'string "(" (read-line) ")")))
         (columns    (length first-line))
         (type       (type-of (car first-line)))
         (matrix     (make-array (list columns columns)
                                 :initial-element (coerce 0 type)
                                 :element-type type)))
    (loop for j from 0 below columns
          for x in first-line
       do (setf (aref matrix 0 j) x))
    (loop for i from 1 below columns
       do (loop for j from 0 below columns
             do (setf (aref matrix i j) (read))))
    matrix))

(defun read-matrix-from-file (file)
  "Returns the square matrix read from FILE."
  (with-open-file (*standard-input* file
                   :direction         :input
                   :if-does-not-exist :error)
    (read-matrix)))

;;;

(defun test-gauss (file &key (verify nil))
  (let ((matrix (read-matrix-from-file file)))
    (time (gauss matrix
                 (make-array (list (array-dimension matrix 1))
                             :initial-element (coerce 0 (array-element-type matrix)))
                 :verify verify))
    nil))

;;;

nil