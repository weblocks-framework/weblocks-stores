
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:weblocks-stores-asd
  (:use :cl :asdf))

(in-package :weblocks-stores-asd)

(defsystem weblocks-stores
   :name "weblocks-stores"
   :maintainer "Olexiy Zamkoviy, Scott L. Burson"
   :author "Olexiy Zamkoviy"
   :version "0.5.1"
   :licence "LLGPL"
   :description "A base for weblocks stores"
   :depends-on (:closer-mop :metatilities :weblocks-util)
   :components 
   ((:module src 
     :components 
     ((:file "package")
      (:module store
       :components ((:file "store-api")
         (:file "store-utils"))
       :depends-on ("package"))))))

