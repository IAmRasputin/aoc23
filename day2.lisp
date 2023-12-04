(in-package #:cl-user)
(uiop:define-package #:aoc23/day2
  (:nicknames #:day2)
  (:use :cl :alexandria :serapeum :aoc23/util)
  (:export :part1 :part2))
(in-package :aoc23/day2)

;; An "estimate" of a bag, containing the minimum number of each color it must have
(defclass bag ()
  ((%red-cubes :initarg :red
               :initform 0
               :accessor red-cubes
               :type integer)
   (%green-cubes :initarg :green
                 :initform 0
                 :accessor green-cubes)
   (%blue-cubes :initarg :blue
                :initform 0
                :accessor blue-cubes)))

(defclass game ()
  ((%id :initarg :id
        :initform (error "Games need an ID")
        :reader game-id)
   (%bag :initarg :bag
         :initform (make-instance 'bag)
         :accessor game-bag)
   (%hints :initarg :hints
           :accessor game-hints)))

(defun hint (game color count)
  (cond
    ((equal color "red") (when (> count (red-cubes bag))
                           (setf (red-cubes bag) count)))
    ((equal color "blue") (when (> count (blue-cubes bag))
                           (setf (blue-cubes bag) count)))
    ((equal color "green") (when (> count (green-cubes bag))
                             (setf (green-cubes bag) count)))
    (t (error "~a is not a creative color" color))))
