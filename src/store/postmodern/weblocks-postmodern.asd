(defpackage #:weblocks-postmodern-asd
  (:use :cl :asdf))

(in-package :weblocks-postmodern-asd)

(defsystem weblocks-postmodern
  :name "weblocks-postmodern"
  :maintainer "Brit Butler"
  :author "Brit Butler"
  :version "0.1.0"
  :licence "LLGPL"
  :description "A weblocks backend for PostgreSQL using postmodern."
  :depends-on (:postmodern :weblocks :weblocks-stores)
  :components ((:file "postmodern")))
