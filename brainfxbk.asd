#|
  This file is a part of brainfxbk project.
  Copyright (c) 2018 leaf_chage
|#

#|
  Author: leaf_chage
|#

(in-package :cl-user)
(defpackage brainfxbk-asdf
  (:use :cl
        :asdf))
(in-package brainfxbk-asdf)

(asdf:defsystem "brainfxbk"
  :version "0.1.0"
  :author "leaf_chage"
  :license ""
  :depends-on (:cl-ppcre
               :split-sequence)
  :components ((:module "src"
                :components
                ((:file "brainfxbk"))))
  :description "")

;# vim:filetype=lisp
