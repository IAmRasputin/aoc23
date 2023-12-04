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
           :initform nil
           :accessor game-hints)))

(defun hint (game color count)
  (cond
    ((equal color "red") (when (> count (red-cubes (game-bag game)))
                           (setf (red-cubes (game-bag game)) count)))
    ((equal color "blue") (when (> count (blue-cubes (game-bag game)))
                            (setf (blue-cubes (game-bag game)) count)))
    ((equal color "green") (when (> count (green-cubes (game-bag game)))
                             (setf (green-cubes (game-bag game)) count)))
    (t (error "~a is not a creative color" color)))
  (push (cons color count) (game-hints game)))

(defun parse-game (line)
  (let* ((game-and-hints (split-sequence #\: line))
         (game-and-id (split-sequence #\Space (first game-and-hints)))
         (hints (split-sequence #\; (second game-and-hints)))
         (game (make-instance 'game :id (parse-integer (second game-and-id)))))
    (dolist (string-hint hints)
      (let ((colors (split-sequence #\, string-hint)))
        (dolist (color colors)
          (let ((parts (split-sequence #\Space (string-trim " " color))))
            (hint game (second parts) (parse-integer (first parts)))))))
    game))

(defun parse-games (lines)
  (let ((games (make-array (1+ (length lines))))) ;; Since the lowest ID is 1, not 0
    (setf (aref games 0) (make-instance 'game :id 0))
    (dolist (line lines)
      (let ((game (parse-game line)))
        (setf (aref games (game-id game)) game)))
    games))

(defun possible (game bag)
  "Is it possible for BAG to be the bag for GAME?"
  (unless (zerop (game-id game))
    (let ((test-bag (game-bag game)))
      (and (<= (red-cubes test-bag) (red-cubes bag))
           (<= (blue-cubes test-bag) (blue-cubes bag))
           (<= (green-cubes test-bag) (green-cubes bag))))))

(defun part1 ()
  (let* ((input (input-lines 2))
         (games (parse-games input))
         (test-bag (make-instance 'bag :red 12 :green 13 :blue 14))
         (possibilities (filter (lambda (g)
                                  (possible g test-bag))
                                games)))
    (apply #'+ (map 'list #'game-id possibilities))))


(defun game-power (game)
  (let ((bag (game-bag game)))
    (* (red-cubes bag)
       (blue-cubes bag)
       (green-cubes bag))))

(defun part2 ()
  (let* ((input (input-lines 2))
         (games (parse-games input)))
    (apply #'+ (map 'list #'game-power games))))
