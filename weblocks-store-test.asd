
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-store-test-asd
  (:use :cl :asdf))

(in-package :weblocks-store-test-asd)

(defsystem weblocks-store-test
  :name "weblocks-store-test"
  :maintainer "Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :version "0.1.1"
  :licence "LLGPL"
  :description "A test suite for weblocks backend stores."
  :depends-on (:weblocks :lift :f-underscore :weblocks-memory :weblocks-test)
  :components 
  ((:module test
    :components
    ((:file "weblocks-store-test")
     (:module store
      :components ((:file "store-utils"))
      :depends-on ("weblocks-store-test"))))))
