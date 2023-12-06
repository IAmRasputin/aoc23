(in-package :cl-user)
(uiop:define-package #:aoc23/util
  (:nicknames #:util)
  (:use :cl :alexandria :serapeum)
  (:export :input-string :input-lines :prompt-to-string :echo-prompt :digit-chars))
(in-package :aoc23/util)

(defvar digit-chars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun input-string (day)
  (let ((filename (asdf:system-relative-pathname :aoc23 (format nil "input/day~d/input" day))))
    (read-file-into-string filename)))

(defun input-lines (day)
  (let ((filename (asdf:system-relative-pathname :aoc23 (format nil "input/day~d/input" day))))
    (uiop:read-file-lines filename)))

(defun prompt-to-string (day &optional (part 1))
  (let* ((filename (asdf:system-relative-pathname :aoc23 (format nil "prompts/day~d/prompt~d" day part)))
         (prompt-string (read-file-into-string filename)))
    prompt-string))

(defun echo-prompt (day &optional (part 1))
  (let* ((filename (asdf:system-relative-pathname :aoc23 (format nil "prompts/day~d/prompt~d" day part)))
         (prompt-string (read-file-into-string filename)))
    (format t "~&~A~&" prompt-string)
    (values)))
