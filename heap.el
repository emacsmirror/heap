
;;; heap.el --- heap (a.k.a. priority queue) data structure package

;; Copyright (C) 2004 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.1
;; Keywords: heap, priority queue

;; This file is part of the Emacs Predictive Completion package.
;;
;; The Emacs Predicive Completion package is free software; you can
;; redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; The Emacs Predicive Completion package is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the Emacs Predicive Completion package; if not, write
;; to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA


;;; Commentary:
;;
;; A heap consists of two cons cells, the first one holding the tag 'HEAP in
;; the car cell and the second one having the heap in the car and the compare
;; function in the cdr cell. The compare function must take two arguments of
;; the type which is to be stored in the heap and must return non-nil or
;; nil. To implement a max-heap, it should return non-nil if the first
;; argument is "greater" than the second. To implement a min-heap, it should
;; return non-nil if the first argument is "less than" the second.
;;
;; Note that this package implements a ternary heap, since ternary
;; heaps are about 12% more efficient than binary heaps for heaps
;; containing more than about 10 elements. And for very small heaps,
;; the difference is negligable.


;;; Change log:
;;
;; version 0.1: initial release



;;; Code:

(provide 'heap)



;;; ================================================================
;;;       Internal functions for use in the heap package


(defmacro hp-vect (heap)
  ;; Return the heap vector.  ;; INTERNAL USE ONLY
  `(car (cdr ,heap))
)



(defmacro hp-cmpfun (heap)
  ;; Return the comparison function of a heap.  ;; INTERNAL USE ONLY
  `(cdr (cdr ,heap))
)



(defmacro hp-set-vect (heap vect)
  ;; Set the vector containing the heap itself to VECT.
  `(setcar (cdr ,heap) ,vect)
)



(defun hp-sift-up (heap n)
  ;; Sift-up starting from element N of the heap vector belonging to
  ;; heap HEAP.  ;; INTERNAL USE ONLY
  (let* ((i n) (j nil) (vect (hp-vect heap)) (v (aref vect n)))
    ;; Keep moving element up until it reaches top or is smaller/bigger
    ;; than its parent.
    (while (and (> i 0)
		(funcall (hp-cmpfun heap) v
			 (aref vect (setq j (/ (1- i) 3)))))
      (vswap vect i j)
      (setq i j)))
)



(defun hp-sift-down (heap n)
  ;; Sift-down from element N of the heap vector belonging to
  ;; heap HEAP.  ;; INTERNAL USE ONLY
  (let* ((vect (hp-vect heap))
	(cmpfun (hp-cmpfun heap))
	(i n) (j (hp-child heap i)) (len (length vect)) (v (aref vect n)))
    
    ;; Keep moving the element down until it reaches the bottom of the tree or
    ;; reaches a position where it is bigger/smaller than all its children.
    (while (and j (funcall cmpfun (aref vect j) v))
      (vswap vect i j)
      (setq i j)
      (setq j (hp-child heap i)))
  )
)



(defmacro hp-child (heap i)
  ;; Compare the 3 children of element I, and return element reference of the
  ;; smallest/largest (depending on whethen it's a min- or max-heap).
  ;; INTERNAL USE ONLY
  `(let* ((vect (hp-vect ,heap))
	(cmpfun (hp-cmpfun ,heap))
	(len (length vect)) (j nil) (k (* 3 ,i)))
     ;; Lots of if's in case I has less than three children.
     (if (>= (1+ k) len) nil
       (if (>= (+ 2 k) len) (1+ k)
	 (setq j (if (funcall cmpfun (aref vect (1+ k)) (aref vect (+ 2 k)))
		(1+ k) (+ 2 k)))
	  (if (>= (+ 3 k) len) j
	    (if (funcall cmpfun (aref vect j) (aref vect (+ 3 k))) j (+ 3 k)))
	  )))
)





;;; ================================================================
;;;         Utility functions used in the heap package

(defmacro vswap (vect i j)
  ;; Swap elements I and J of vector VECT.
  `(let ((tmp (aref ,vect ,i)))
     (aset ,vect ,i (aref ,vect ,j))
     (aset ,vect ,j tmp) ,vect)
)




;;; ================================================================
;;;          The public functions which operate on heaps.


(defun heap-create (compare-function)
  "Create an empty heap using COMPARE-FUNCTION as the comparison
function. COMPARE-FUNCTION takes two arguments, A and B, and returns non-nil
or nil. To implement a max-heap, it should return non-nil if A is greater than
B. To implemenet a min-heap, it should return non-nil if A is less than B."
  (cons 'HEAP (cons [] compare-function))
)


(defun heap-copy (heap)
  "Return a copy of heap HEAP."
  (let ((newheap (heap-create (hp-cmpfun heap))))
    (hp-set-vect newheap (hp-vect heap))
    newheap)
)


(defun heap-p (obj)
  "Return t if OBJ is a heap, nil otherwise."
  (eq (car-safe obj) 'HEAP)
)



(defun heap-empty (heap)
  "Return t if the heap is empty, nil otherwise."
  (= 0 (length (hp-vect heap)))
)



(defun heap-size (heap)
  "Return the number of entries in the heap."
  (length (hp-vect heap))
)



(defun heap-compare-function (heap)
  "Return the comparison function for the heap HEAP."
  (hp-cmpfun heap)
)



(defun heap-add (heap data)
  "Add DATA to the heap."
  ;; Add data to bottom of heap and sift-up from bottom.
  (hp-set-vect heap (vconcat (hp-vect heap) (vector data)))
  (hp-sift-up heap (1- (length (hp-vect heap))))
)



(defun heap-delete-root (heap)
  "Return the root of the heap and delete it from the heap."
  (let ((vect (hp-vect heap))
	(root nil) (len nil))
    
    ;; Deal with special cases of empty heap and heap with just one element.
    (if (heap-empty heap) nil
      (setq len (length vect))
      (setq root (aref vect 0))
      (if (= 1 len) (hp-set-vect heap [])
	;; Delete root, swap last element to top, and sift-down from top.
	(hp-set-vect heap (vconcat (vector (aref vect (1- len)))
				   (subseq vect 1 (1- len))))
	(hp-sift-down heap 0))
      root)
  )
)



(defun heap-modify (heap match-function data)
  "Replace the first heap entry identified by MATCH-FUNCTION with DATA, if a
match exists. Return t if there was a match, nil otherwise.

The function MATCH-FUNCTION should take one argument of the type stored in the
heap, and return non-nil if it should be modified, nil otherwise.

Note that only the match highest up the heap is modified."
  
  (let ((vect (hp-vect heap)) (i 0))
    ;; search vector for the first match
    (while (and (< i (length vect))
		(not (funcall match-function (aref vect i))))
      (setq i (1+ i)))
    ;; if a match was found, modify it
    (if (< i (length vect))
	(let ((olddata (aref vect i)))
	  (aset vect i data)
	  ;; if the new data is greater than old data, sift-up, otherwise
	  ;; sift-down
	  (if (funcall (hp-cmpfun heap) data olddata)
	      (hp-sift-up heap i)
	    (hp-sift-down heap i))
	  t  ; return t if the match was successfully modified
	)
      nil  ; return nil if no match was found
    )
  )
)


;;; heap.el ends here
