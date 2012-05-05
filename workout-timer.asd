;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-
;;; workout-timer, trivial work out timer in CL
;;;
;;; Free Software available under an MIT-style license.
;;; Copyright (c) 2012 - 2012, Francois-Rene Rideau

(defsystem :workout-timer
  :licence "MIT"
  :description "Bundle operations for ASDF"
  :long-description "Can bundle one or many asdf systems into one .fasl and/or one .so"
  :depends-on (:asdf :mixalot :mixalot-vorbis :local-time)
  :components
  ((:file "package")
   (:file "specials" :depends-on ("package"))
   (:file "timer" :depends-on ("specials"))))
