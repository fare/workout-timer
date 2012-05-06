;;; -*- Mode: Lisp ; Syntax: ANSI-Common-Lisp -*-
;;; workout-timer, trivial work out timer in CL
;;;
;;; Free Software available under an MIT-style license.
;;; Copyright (c) 2012 - 2012, Francois-Rene Rideau

#| Use it as follows:

sbcl --load ~/quicklisp/setup \
  --eval '(ql:quickload "workout-timer")' \
  --eval '(workout-timer::start)'
  --eval '(sb-ext:quit :recklessly-p t :unix-status 0)'

Or, from the SBCL (SLIME) REPL:

  (load "~/quicklisp/setup")
  (ql:quickload "workout-timer")
  (workout-timer::start)

|#

(defsystem :workout-timer
  :licence "MIT"
  :description "Bundle operations for ASDF"
  :long-description "Can bundle one or many asdf systems into one .fasl and/or one .so"
  :depends-on (:asdf :mixalot :mixalot-vorbis :local-time)
  :components
  ((:file "package")
   (:file "specials" :depends-on ("package"))
   (:file "timer" :depends-on ("specials"))))
