(uiop:define-package :workout-timer/finalizer
  (:nicknames :workout-timer/finalizer)
  (:use :common-lisp :asdf :uiop :workout-timer/timer))

(in-package :workout-timer/finalizer)

;; Mark all systems as preloaded, even workout-timer:
;; otherwise dynamically looking for the base directory to the system-relative-pathname,
;; will cause seconds to be wasted at start time.
(defun register-systems-as-immutable ()
  (map () 'register-immutable-system
       (remove "workout-timer"
               (already-loaded-systems) :test 'equal :key 'primary-system-name)))

(register-image-dump-hook 'register-systems-as-immutable)
