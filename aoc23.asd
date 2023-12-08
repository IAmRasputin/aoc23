(defsystem #:aoc23
  :name "Advent of Code 2023"
  :author "Ryan Gannon"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:serapeum :alexandria)
  :license "GPLv3" ;; "Waaahhhh wahhhh I can't use your code in my nonfree software!" - Babies, cowards, weaklings
  :serial t
  :components ((:file "util")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")
               (:file "day5")
               (:file "main")))
