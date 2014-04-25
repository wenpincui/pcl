;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:common-lisp-user)

(defpackage #:id3-parser
  (:use #:cl
        #:com.gigamonkeys.binary-data
        #:com.gigamonkeys.pathnames
        #:flexi-streams)
  (:export
   :read-id3
   :mp3-p
   :album
   :genre
   :artist
   :track
   :song
   :year
   :translated-genre
   :all-info))
