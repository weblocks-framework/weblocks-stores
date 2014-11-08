
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-perec-asd
  (:use :cl :asdf))

(in-package :weblocks-perec-asd)

(defsystem weblocks-perec
  :name "weblocks-perec"
  :maintainer "Olexiy Zamkoviy"
  :author "Olexiy Zamkoviy"
  :version "0.0.2"
  :licence "LLGPL"
  :description "A weblocks backend for cl-perec."
  :depends-on (:hu.dwim.perec :weblocks-stores)
  :components ((:file "perec")))
