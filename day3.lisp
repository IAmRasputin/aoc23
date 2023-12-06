(in-package #:cl-user)
(uiop:define-package #:aoc23/day3
  (:nicknames #:day3)
  (:use :cl :alexandria :serapeum :aoc23/util)
  (:export :part1 :part2))
(in-package :aoc23/day3)

;; The "schematic" is represented as a list of strings

(defun get-number (schematic row col)
  "Returns the found number (or nil), and its starting/ending column indicies (inclusive)"
  (when (find (aref (nth row schematic) col) digit-chars)
    (if (and (not (zerop col)) (find (aref (nth row schematic) (- col 1)) digit-chars))
        (get-number schematic row (- col 1))
        (let ((parsed (parse-integer (nth row schematic) :start col :junk-allowed t)))
          (values parsed
                  col
                  (+ col (length (format nil "~d" parsed))))))))

(defun get-char (schematic row col)
  (aref (nth row schematic) col))

(defun adjacent (schematic row col &key get-coords)
  (multiple-value-bind (num start end) (get-number schematic row col)
    (let* ((coords-raw (loop for r from (- row 1) upto (+ row 1)
                             collect
                             (loop for c from (if num
                                                  (- start 1)
                                                  (- col 1))
                                     upto (if num
                                              end
                                              (+ col 1))
                                   collect (cons r c))))
           (coords-flattened (append (first coords-raw)
                                     (second coords-raw)
                                     (third coords-raw)))
           (filtered (remove-if (lambda (coord)
                                  (or (if num
                                          (and (eq (car coord) row)
                                               (<= start (cdr coord) (- end 1)))
                                          (equal coord (cons row col)))
                                      (< (car coord) 0)
                                      (< (cdr coord) 0)
                                      (>= (car coord) (length schematic))
                                      (>= (cdr coord) (length (car schematic)))))
                                coords-flattened)))
      (if get-coords
          filtered
          (mapcar (lambda (c)
                 (get-char schematic (car c) (cdr c)))
               filtered)))))

(defun is-part-number (schematic row col)
  (when (get-number schematic row col)
    (let* ((adj (adjacent schematic row col))
           (no-blanks (remove #\. adj))
           (no-numbers (remove-if #'digit-char-p no-blanks)))
      (< 0 (length no-numbers)))))

(defun is-gear (schematic row col)
  (when (eq (get-char schematic row col) #\*)
    (let ((adj-coords (adjacent schematic row col :get-coords t))
          ignored-coords
          nearby-numbers) ;; Coordinates of adjacent numbers, so we don't treat different digits as discrete nums
      (mapc (lambda (coord)
              (unless (member coord ignored-coords :test #'equal)
                (multiple-value-bind (num start end) (get-number schematic (car coord) (cdr coord))
                  (when num
                    (loop for c from start to end do
                      (unless (member (cons (car coord) c) ignored-coords :test #'equal)
                        (push (cons (car coord) c)ignored-coords)))
                    (push num nearby-numbers)))))
            adj-coords)
      (when (= (length nearby-numbers) 2)
        nearby-numbers))))

(defun part1 ()
  (let* ((input (input-lines 3)))
    (do ((row 0)
         (col 0)
         (sum 0))
        ((<= (length input) row) sum)
      (multiple-value-bind (num start end) (get-number input row col)
        (if num
            (progn
              (when (is-part-number input row col)
                (incf sum num))
              (incf col (- end start)))
            (incf col))
        (when (<= (length (car input)) col)
          (setf col 0)
          (incf row))))))

(defun part2 ()
  (let* ((input (input-lines 3)))
    (do ((row 0)
         (col 0)
         (sum 0)
         winners)
        ((<= (length input) row) (progn
                                   (format t "~s~%" (nreverse winners))
                                   sum))
      (let ((result (is-gear input row col)))
        (format t "peekin @ (~d ~d) == ~a~%" row col result)
        (when result
          (incf sum (apply #'* result))
          (push (cons row col) winners))
        (incf col)
        (when (<= (length (car input)) col)
          (setf col 0)
          (incf row))))))
