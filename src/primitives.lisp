(in-package :numericals.internals)

(defparameter *type* 'single-float
  ;; better way to restrict the value?
  "Can only be one of FIXNUM, SINGLE-FLOAT, or DOUBLE-FLOAT.")

(defun split-at-keywords (args)
  "Example: (1 2 3 :a 2 :b 3) => ((1 2 3) (:a 2 :b 3))"
  (if args
      (if (keywordp (car args))
          (list () args)
          (destructuring-bind (non-keyword-args keyword-args)
              (split-at-keywords (cdr args))
            (list (cons (car args) non-keyword-args)
                  keyword-args)))
      '(() ())))

(defun array-initial-element (type)
  (ecase type
    (single-float 0.0)
    (double-float 0.0d0)
    (fixnum 0)))

;;; Should error handling be improved?
(defun %cast (to-type object)
  (if (eq to-type 'fixnum)
      (nth-value 0 (floor object))
      (coerce object to-type)))

(defun map-tree (function tree)
  (when tree
    (if (listp tree)
        (loop for tr in tree
           collect (map-tree function tr))
        (funcall function tree))))

(defun cast (to-type object)
  (if (arrayp object)
      (nu:astype object to-type)
      (map-tree (curry '%cast to-type) object)))

;;; Possibly do this using SIMD
(defun nu:astype (array type)
  (let* ((storage-vector (1d-storage-array array))
         (return-array (make-array (array-dimensions array)
                                   :element-type type
                                   :initial-element (array-initial-element type)))
         (ra-storage-vector (1d-storage-array return-array)))
    (loop for i fixnum from 0 below (length storage-vector)
       do (setf (aref ra-storage-vector i)
                (cast type (aref storage-vector i)))
       finally (return return-array))))

(defun nu:shape (array-like)
  (cond ((arrayp array-like)
         (array-dimensions array-like))
        ((listp array-like)
         (cons (length array-like)
               (nu:shape (car array-like))))
        (t nil)))

(defun nu:zeros (&rest args)
  "ARGS: (&rest array-dimensions &key (type *type*))
Examples: 
  (zeros 2 3)
  (zeros 3 3 :type 'fixnum)"
  (destructuring-bind (dimensions (&key (type *type*))) (split-at-keywords args)
    (make-array dimensions :element-type type
                :initial-element (cast type 0))))

(defun nu:asarray (array-like &key (type *type*))
  (make-array (nu:shape array-like)
              :element-type type
              :initial-contents (cast type array-like)))
