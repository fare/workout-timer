#+xcvb (module (:depends-on ("specials")))

(in-package :workout-timer)

(defparameter *version* "1.0.1")

(defparameter +workout-timer-option-spec+
  '((("work-seconds" #\w) :type integer :optional t :documentation "duration of each exercise, in seconds")
    (("pause-seconds" #\p) :type integer :optional t :documentation "duration of pause between exercises, in seconds")
    (("exercise" #\e) :action :exercises :type string :list t :optional t :documentation "name of an exercise (repeat option as needed)")
    (("help" #\h) :type boolean :optional t :documentation "display help")
    (("version" #\V) :type boolean :optional t :documentation "display version")))

(defun main (args)
  (handle-command-line +workout-timer-option-spec+ 'workout-timer
                       :command-line args :name "workout-timer" :rest-arity nil))

(defun show-version ()
  (format t "workout-timer ~a~%" *version*))

(defun show-help ()
  (show-version)
  (show-option-help +workout-timer-option-spec+ :sort-names t))


(defun pn (x &optional type) (asdf:system-relative-pathname :workout-timer x :type type))

(defun oggfile (x) (namestring (pn x "ogg")))

(defun oggstream (x) (make-vorbis-streamer (oggfile x) :output-rate 44100))

(defun addogg (x) (mixer-add-streamer *mixer* (oggstream x)))

;; Samples from the respective URLs, as recoded with sox ${in} -r 44100 ${out}.ogg
(defun bell () (addogg "bell")) ;; https://www.freesound.org/people/Taira%20Komori/sounds/212057/
(defun click () (addogg "click")) ;; https://www.freesound.org/people/EdgardEdition/sounds/113634/
(defun buzzer () (addogg "buzzer")) ;; https://www.freesound.org/people/guitarguy1985/sounds/54047/
(defun gong () (addogg "gong")) ;; https://www.freesound.org/people/Veiler/sounds/207168/

(defun integer-digits (n &optional (base 10))
  "Given a non-negative integer N, return how many digits it takes to write n in given BASE"
  (floor (log (1+ n) base)))

(defun vsleep (&optional n)
  (let ((current-time (now)))
    (when (or (null n)
              (null *current-time*)
              (when (timestamp< current-time *current-time*)
                (format! *error-output* "~&Your clock ran backwards. Adjusting.~%")
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
         ;;(format! *error-output* "~&Oops, skipped a beat~%")
         (setf *current-time* current-time))))))

(defun countdown (n &key click)
  (loop :with l = (integer-digits n)
        :for i :from n :downto 0 :do
    (format! t "  ~v,' D  ~C" l i #\return)
    (unless (zerop i)
      (when click (click))
      (vsleep 1))))

(defun integer-length* (integer &optional (base 10))
  (cond ((= base 2)
         (integer-length integer))
        ((= base (ash 1 (1- (integer-length base))))
         (values (ceiling (integer-length integer) (1- (integer-length base)))))
        (t (values (ceiling (log (1+ integer) base))))))

;; See exercises here:
;; http://well.blogs.nytimes.com/2013/05/09/the-scientific-7-minute-workout/
;; http://graphics8.nytimes.com/images/2013/05/12/health/12well_physed/12well_physed-tmagArticle.jpg
;; https://www.youtube.com/watch?v=ECxYJcnvyMw
(defun my-workout (&key work-seconds pause-seconds exercises)
  (vsleep 0)
  (loop
    :with work-seconds = (or work-seconds 30)
    :with pause-seconds = (or pause-seconds 10)
    :with exercises = (or exercises
                          '("Jumping jack" "Wall sit" "Push-up" "Abdominal crunch"
                            "Step-up onto chair" "Squat" "Triceps dip on chair" "Plank"
                            "High knees running in place" "Lunge" "Push-up and rotation" "Side plank"))
    :with total = (length exercises)
    :with il = (integer-length* total 10)
    :for (exercise . morep) :on exercises
    :for count :from 1 :do
    (format! t "[~v,'0D/~D] ~As!~%" il count total exercise)
    (bell)
    (countdown work-seconds :click t)
    (cond
      (morep (buzzer)
             (format! t "Rest. (Next: ~As.)~%" (car morep))
             (countdown pause-seconds :click nil))
      (t (gong) (format! t "Done!~%") (sleep 5) (return)))))

(defun mix-it (&key work-seconds pause-seconds exercises)
  (main-thread-init) ;; mixalot is well-designed enough not to do it twice
  (let ((*mixer* (create-mixer :rate 44100)))
    (my-workout :work-seconds work-seconds :pause-seconds pause-seconds :exercises exercises)
    (destroy-mixer *mixer*))
  (values))

(defun show-picture ()
  ;; Save as pic.jpg the picture from
  ;; http://graphics8.nytimes.com/images/2013/05/12/health/12well_physed/12well_physed-tmagArticle.jpg
  (run-program (format nil "qiv --center ~A &" (native-namestring (pn "pic.jpg")))))

(defun workout-timer (&key work-seconds pause-seconds exercises help version)
  (cond
    (version (show-version))
    (help (show-help))
    (t (mix-it :work-seconds work-seconds :pause-seconds pause-seconds :exercises exercises))))
