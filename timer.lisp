#+xcvb (module (:depends-on ("specials")))

(in-package :workout-timer)

(defun oggfile (x)
  (namestring (asdf:system-relative-pathname :workout-timer (asdf:strcat x ".ogg"))))

(defun oggstream (x)
  (make-vorbis-streamer (oggfile x) :output-rate 44100))

(defun addogg (x)
  (mixer-add-streamer *mixer* (oggstream x)))

(defun bell ()
  (addogg "bell"))
(defun click ()
  (addogg "click"))
(defun buzzer ()
  (addogg "buzzer"))
(defun gong ()
  (addogg "gong"))
  

(defun integer-digits (n &optional (base 10))
  "Given a non-negative integer N, return how many digits it takes to write n in given BASE"
  (floor (log (1+ n) base)))

(defun vsleep (&optional n)
  (let ((current-time (now)))
    (when (or (null n)
              (null *current-time*)
              (when (timestamp< current-time *current-time*)
                (format *error-output* "~&Your clock ran backwards. Adjusting.~%")
                t))
      (setf *current-time* current-time))
    (when (null n)
      (setf n 0))
    (assert (>= n 0))
    (let* ((target-time (timestamp+ *current-time* (floor (* n 1000000000)) :nsec))
           (delay (timestamp-difference target-time current-time)))
      (cond
        ((> delay 0)
         (sleep delay)
         (setf *current-time* target-time))
        (t
         ;; already missed the deadline, skipping.
         (format *error-output* "~&Oops, skipped a beat~%")
         (setf *current-time* current-time))))))

(defun countdown (n &key click)
  (loop :with l = (integer-digits n)
        :for i :from n :downto 0 :do
    (format t "  ~v,' D~C" l i #\return)
    (unless (zerop i)
      (when click (click))
      (vsleep 1))))

(defun my-workout ()
  (vsleep 0)
  (loop :for i :from 0 :do
    (bell)
    (countdown 25 :click t)
    (buzzer)
    (if (< i 4)
      (vsleep 5)
      (progn (gong) (return)))))

(defun mix-it ()
  (setf *mixer* (create-mixer))
  (my-workout)
  (destroy-mixer *mixer*)
  (setf *mixer* nil)
  (values))

(defun start ()
  (main-thread-init)
  (mix-it))
