(in-package #:cl-user)
(uiop:define-package #:aoc23/day1
  (:nicknames #:day1)
  (:use :cl :alexandria :serapeum :aoc23/util)
  (:export :part1 :part2))
(in-package :aoc23/day1)

(defun part-1-calibration-value (code)
  (let* ((digits (remove-if #'alpha-char-p code))
         (tens (* 10 (digit-char-p (char digits 0))))
         (ones (digit-char-p (char digits (- (length digits) 1)))))
    (+ tens ones)))

(defun part1 ()
  (let ((input (input-lines 1)))
    (apply #'+ (mapcar #'part-1-calibration-value input))))

(defun minimum (number &rest more-numbers)
  (apply #'min (remove-if #'null (cons number more-numbers))))

(let ((words '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
      (digits '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
      (digit-map (dict "one" 1
                       "two" 2
                       "three" 3
                       "four" 4
                       "five" 5
                       "six" 6
                       "seven" 7
                       "eight" 8
                       "nine" 9)))
  (defun first-number (code)
    (let (min-idx token)
      (mapc (lambda (num)
              (let* ((found? (search num code)))
                (if min-idx
                    (when (and found? (< found? min-idx))
                      (setf min-idx found?)
                      (setf token num))
                    (when found?
                      (setf min-idx found?)
                      (setf token num)))))
            (append words digits))
      (cond
        ((member token words :test #'equal) (nth-value 0 (@ digit-map token)))
        ((member token digits :test #'equal) (nth-value 0 (parse-integer token)))
        (t nil))))
  (defun last-number (code)
    (let (max-idx token)
      (mapc (lambda (num)
              (let* ((found? (search num code :from-end t)))
                (if max-idx
                    (when (and found? (> found? max-idx))
                      (setf max-idx found?)
                      (setf token num))
                    (when found?
                      (setf max-idx found?)
                      (setf token num)))))
            (append words digits))
      (cond
        ((member token words :test #'equal) (nth-value 0 (@ digit-map token)))
        ((member token digits :test #'equal) (nth-value 0 (parse-integer token)))
        (t nil)))))

(defun part-2-calibration-value (code)
  (+ (* 10 (first-number code)) (last-number code)))

(defun part2 ()
  (let ((input (input-lines 1)))
    (apply #'+ (mapcar #'part-2-calibration-value input))))
