(in-package #:cl-user)
(uiop:define-package #:aoc23/day5
  (:nicknames #:day5)
  (:use :cl :alexandria :serapeum :aoc23/util)
  (:import-from :uiop :split-string)
  (:export :part1 :part2))
(in-package :aoc23/day5)

(defvar *input* (input-lines 5))
