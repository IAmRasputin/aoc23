(in-package #:cl-user)
(uiop:define-package #:aoc23/day1
  (:nicknames #:day1)
  (:use :cl :alexandria :serapeum :aoc23/util)
  (:export :part1 :part2))
(in-package :aoc23/day1)

(defun part-1-broken-calibration-value (code)
  (let* ((digits (remove-if #'alpha-char-p code))
         (tens (* 10 (digit-char-p (char digits 0))))
         (ones (digit-char-p (char digits (- (length digits) 1)))))
    (+ tens ones)))

(defun calibration-value (code)
  (format t "~a -> " code)
  (let* ((digits (remove-if #'alpha-char-p (digitize code)))
         (tens (* 10 (digit-char-p (char digits 0))))
         (ones (digit-char-p (char digits (- (length digits) 1)))))
    (format t "~a ( ~d )~%" digits (+ tens ones))
    (force-output)
    (+ tens ones)))

(defun part1 ()
  (let ((input (input-lines 1)))
    (apply #'+ (mapcar #'part-1-broken-calibration-value input))))

(defun digitize (code)
  (do ((digit-key (dict "one" "1"
                        "two" "2"
                        "three" "3"
                        "four" "4"
                        "five" "5"
                        "six" "6"
                        "seven" "7"
                        "eight" "8"
                        "nine" "9"))
       (idx 0 (+ 1 idx))
       (fixed code))
      ((<= (length fixed) idx) fixed)
    (mapc (lambda (key)
            (when (string^= key fixed :start2 idx)
              (setf fixed (string-replace key fixed (@ digit-key key)))))
          (hash-table-keys digit-key))))


(defun part2 ()
  (let ((input (input-lines 1)))
    (apply #'+ (mapcar #'calibration-value input))))
