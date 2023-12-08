(in-package #:cl-user)
(uiop:define-package #:aoc23/day4
  (:nicknames #:day4)
  (:use :cl :alexandria :serapeum :aoc23/util)
  (:import-from :uiop :split-string)
  (:export :part1 :part2))
(in-package :aoc23/day4)

(defvar *input* (input-lines 4))

(defun parse-integer-list (s)
  (mapcar #'parse-integer
          (remove ""
                  (split-string s)
                  :test #'equal)))

(defun calculate-winners (my-numbers winning-numbers)
  (let (winners)
    (dolist (n my-numbers)
      (when (member n winning-numbers)
        (push n winners)))
    (nreverse winners)))


(defun part-1-score-card (card-string)
  (let* ((split-on-pipe (mapcar (lambda (s)
                                  (string-trim " " s))
                                (cdr (split-string card-string :separator '(#\: #\|)))))
         (winning-numbers (parse-integer-list (first split-on-pipe)))
         (my-numbers (mapcar #'parse-integer
                             (remove ""
                                     (split-string (second split-on-pipe))
                                     :test #'equal)))
         (winners (calculate-winners my-numbers winning-numbers)))
    (if winners
        (expt 2 (- (length winners) 1))
        0)))

(let ((winnings-cache (make-hash-table)))
  (defun part-2-score-card (game-id)
    (let* ((card-string (nth (- game-id 1) *input*))
           (split-on-pipe (mapcar (lambda (s)
                                    (string-trim " " s))
                                  (split-string card-string :separator '(#\: #\|))))
           (game-id (parse-integer (string-trim " " (subseq (first split-on-pipe) 4))))
           (winning-numbers (parse-integer-list (second split-on-pipe)))
           (my-numbers (mapcar #'parse-integer
                               (remove ""
                                       (split-string (third split-on-pipe))
                                       :test #'equal)))
           (winnings
             (or (gethash game-id winnings-cache)
                 (setf (gethash game-id winnings-cache)
                       (loop for i from (1+ game-id) to (min (length *input*)
                                                             (+ game-id (length (calculate-winners my-numbers winning-numbers))))
                             collect i)))))
      winnings)))

(defun part1 ()
  (let ((input (input-lines 4)))
    (apply #'+ (mapcar #'part-1-score-card input))))

(defun part2 ()
  (let ((counts (make-array (1+ (length *input*)) :initial-element 1)))
    (setf (aref counts 0) 0) ;; There are no zero-cards
    (loop for i from 1 to (length *input*) do
      (let ((winnings (part-2-score-card i)))
        (dotimes (x (aref counts i))
          (dolist (won winnings)
            (incf (aref counts won))))))
    (let ((sum 0))
      (loop for i across counts do (incf sum i))
      sum)))
