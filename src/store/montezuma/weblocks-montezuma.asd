
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-montezuma-asd
  (:use :cl :asdf))

(in-package :weblocks-montezuma-asd)

(defsystem weblocks-montezuma
  :name "weblocks-montezuma"
  :maintainer "Olexiy Zamkoviy"
  :author "Olexiy Zamkoviy"
  :version "0.0.5"
  :licence "LLGPL"
  :description "A weblocks backend for montezuma."
  :depends-on (:montezuma :weblocks-stores)
  :components ((:file "montezuma")))

