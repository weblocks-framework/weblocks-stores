
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-custom-asd
  (:use :cl :asdf))

(in-package :weblocks-custom-asd)

(defsystem weblocks-custom
  :name "weblocks-custom"
  :maintainer "Olexiy Zamkoviy, Scott L. Burson"
  :author "Olexiy Zamkoviy"
  :version "0.0.4"
  :licence "LLGPL"
  :description "A weblocks backend for custom store for any object."
  :depends-on (:weblocks-stores :weblocks-memory)
  :components 
  ((:file "custom-store")
   (:file "custom"
    :depends-on ("custom-store"))))

