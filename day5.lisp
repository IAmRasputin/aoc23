(in-package #:cl-user)
(uiop:define-package #:aoc23/day5
  (:nicknames #:day5)
  (:use :cl :alexandria :serapeum :aoc23/util)
  (:import-from :uiop :split-string)
  (:export :part1 :part2))
(in-package :aoc23/day5)

(defvar *input* (input-lines 5))

(defun input-seeds ()
  (let ((numbers (string-trim " " (second (split-string (first *input*) :separator '(#\:))))))
    (mapcar #'parse-integer (split-string numbers))))


(defmacro compile-range (first-start second-start range-size)
  `(lambda (input)
     (when (<= ,first-start ,input ,(+ first-start range-size))
       (+ ,second-start (- input ,first-start)))))

(defun input-maps (from to)
  (let* ((map-strings (cdr (split-sequence-if #'blankp *input*)))
         ())
    ))

(defun part1 ()
  )
