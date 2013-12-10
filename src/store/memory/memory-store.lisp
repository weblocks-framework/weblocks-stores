
(defpackage #:weblocks-memory
  (:use :cl :metabang.utilities :weblocks-stores)
  (:import-from :weblocks-util #:slot-value-by-path)
  (:export :make-scratch-store :objects-from-scratch-store
          :order-objects-in-memory :strictly-less-p :equivalentp
          :range-objects-in-memory 
          :memory-store)
  (:documentation
   "A driver for weblocks backend store API that uses memory and has
   no disk backing. This is useful for prototyping, testing, temporary
   operations, etc."))

(in-package :weblocks-memory)

