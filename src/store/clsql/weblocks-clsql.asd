
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-clsql-asd
  (:use :cl :asdf))

(in-package :weblocks-clsql-asd)

(defsystem weblocks-clsql
  :name "weblocks-clsql"
  :maintainer "Olexiy Zamkoviy, Scott L. Burson"
  :author "Slava Akhmechet"
  :version "0.1.1"
  :licence "LLGPL"
  :description "A weblocks backend for clsql."
  :depends-on (:closer-mop :metatilities :clsql :clsql-fluid :weblocks-stores)
  :components ((:file "clsql")))

