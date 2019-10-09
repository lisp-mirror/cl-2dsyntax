;;; Common Lisp reader macro for the indentation-sensitive syntax.
;;; Author: Goheeca	Date: 20 August 2012
;;; Tested under SBCL, CLISP, and ABCL.
;;; This software is in the public domain.

(defpackage :cl-2dsyntax
  (:use :common-lisp)
  (:export :enable-syntax))

(in-package :cl-2dsyntax)

(defmacro mask-rt ((&rest args) &body body)
  `(let ((*readtable* (copy-readtable ,@args)))
     ,@body))

(defmacro in-syntax (rt-expr)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* ,rt-expr)))

(defun explode (string char)
  (let ((pos (position char string :test #'char=)))
    (if (not pos) (list string)
	(cons (subseq string 0 pos)
	      (explode (subseq string (1+ pos)) char)))))

(defun concat (list)
  (apply #'concatenate 'string list))

(defun implode (strings char &key start end)
  (concat (append
	   (list (if start `(,char)))
	   (loop for c = nil then `(,char)
	      for str in strings
	      collect c collect str)
	   (list (if end `(,char))))))

(defun repeat-char (rep char)
  (concatenate 'string (make-array rep :initial-element char)))

(defun empty-string-p (string &optional (ws '(#\Space)))
  (loop for char across string
     always (member char ws :test #'char=)))

(defun trim (strings &optional (test #'empty-string-p))
  (loop for str in strings unless (funcall test str) collect str))

(defun fetch-from-stream (&optional (stream *standard-input*) (ws '(#\Space)))
  (loop for line = (read-line stream nil nil)
     for first = (not (empty-string-p line)) then t
     while line collect line
     while (or (not first) (member (peek-char nil stream nil nil) ws))))

(defun count-indent (string &optional (ws '(#\Space)))
  (loop for char across string while (member char ws) count t))

(defun ordinal-to-cardinal (string char index)
  (loop repeat (1+ index) 
     for last-pos = 0 then (1+ pos)
     for pos = (position char string :start last-pos :test #'char=)
     while pos finally (return pos)))

(defun number-of (string char)
  (loop for c across string when (char= c char) count t))

(defun read-rest (stream)
  (concat (loop for line = (read-line stream nil nil)
	     while line collect line)))

(defun find-invocation (string char)
  (mask-rt (nil)
	   (let ((*read-suppress* t)
		 (num (number-of string char))
		 (ret))
	     (set-macro-character char
				  #'(lambda (s c)
				      (if (null ret)
					  (setq ret (1+ (ordinal-to-cardinal string c (- num 1 (number-of (read-rest s) c))))))
				      (values)))
	     (set-macro-character #\, #'(lambda (s c) (declare (ignore s c)) (values)))
	     (loop for last-pos = 0 then pos
		for (nil pos) = (multiple-value-list (ignore-errors (read-from-string string nil nil :start last-pos)))
		until ret until (= pos last-pos))
	     ret)))

(defun find-invocation-2d (strings char)
  (let ((pos (find-invocation (implode strings #\Newline :start t) char)))
    (if pos (loop for len in (mapcar #'(lambda (x) (1+ (length x))) strings)
	       for i from 0 while (< (+ ret len) pos)
	       sum len into ret finally (return (list i (- pos 1 ret)))))))

(defun unpush (threshold list)
  (let ((new (remove-if #'(lambda (elem) (>= elem threshold)) list)))
    (list (- (length list) (length new)) new)))

(defun simple-edit-chunk (strings)
  (let* ((lines (trim strings))
	 (inds (mapcar #'count-indent lines))
	 (l-inds)
	 (add-outer (loop for ind in (rest inds)
		       thereis (= ind (first inds))))
	 (ret (concat 
	       (loop for line in lines
		  and ind in inds
		  and next-ind in (append (cdr inds) '(0))
		  for r-ind = (- next-ind ind)
		  when (> r-ind 0) do (push ind l-inds)
		  and append `((#\() ,line (#\Newline))
		  when (<= r-ind 0) append
		    `(,line (#\Newline)
			    ,(repeat-char (let ((new (unpush next-ind l-inds)))
					    (setq l-inds (cadr new))
					    (car new))
					  #\)))))))
    (if (or (= 1 (length lines)) add-outer)
	(concat `((#\() ,ret (#\))))
	ret)))

(defun get-inner (strings)
  (loop with main-ind = (count-indent (first strings))
     for line in strings
     until (< (count-indent line) main-ind)
     collect (pop strings) into inner
     finally (return (list inner strings))))

(defun edit-inner (strings pos fname)
  (let* ((parts (loop with (row col) = pos 
		   for line in strings
		   for i from 0
		   when (< i row) collect line into start
		   when (= i row) collect (subseq line 0 (1- col)) into start
		   and collect (concat (list (repeat-char col #\Space) (subseq line col))) into end
		   when (> i row) collect line into end
		   finally (return (list start end))))
	 (inner (get-inner (second parts))))
    (setf (first (last (first parts))) (concat (list (first (last (first parts)))
						     (funcall fname (first inner)))))
    (append (first parts) (second inner))))

(defun edit-chunk (strings)
  (loop for inv = (find-invocation-2d strings #\!)
     while inv do (setq strings (edit-inner strings inv #'edit-chunk))
     finally (return (simple-edit-chunk strings))))  

(defun main (&optional (stream *standard-input*))
  (let ((chunk (fetch-from-stream stream)))
    (prog1 (read-from-string (edit-chunk chunk)))))

(defun setup (&optional (rt *readtable*))
  (set-macro-character #\! #'(lambda (s c) (declare (ignore c)) (main s)) nil rt)
  rt)

(defvar *my-readtable* (setup (copy-readtable nil)))

(defmacro enable-syntax () (in-syntax *my-readtable*))
