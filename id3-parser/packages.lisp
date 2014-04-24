;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:common-lisp-user)

(defpackage #:id3-parser
  (:use #:cl
        #:com.gigamonkeys.binary-data
        #:com.gigamonkeys.pathnames
        #:flexi-streams))
