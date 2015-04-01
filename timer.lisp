#+xcvb (module (:depends-on ("specials")))

(in-package :workout-timer)

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

;; See exercises here:
;; http://well.blogs.nytimes.com/2013/05/09/the-scientific-7-minute-workout/
;; http://graphics8.nytimes.com/images/2013/05/12/health/12well_physed/12well_physed-tmagArticle.jpg
;; https://www.youtube.com/watch?v=ECxYJcnvyMw
(defun my-workout (&key
                     (work-seconds 30)
                     (pause-seconds 10)
                     (exercises
                      '("Jumping jack" "Wall sit" "Push-up" "Abdominal crunch"
                        "Step-up onto chair" "Squat" "Triceps dip on chair" "Plank"
                        "High knees running in place" "Lunge" "Push-up and rotation" "Side plank")))
  (vsleep 0)
  (loop :for (exercise . morep) :on exercises :do
    (format! t "~As!~%" exercise)
    (bell)
    (countdown work-seconds :click t)
    (cond
      (morep (buzzer)
             (format! t "Rest. (Next: ~As.)~%" (car morep))
             (countdown pause-seconds :click nil))
      (t (gong) (format! t "Done!~%") (sleep 5) (return)))))

(defun mix-it ()
  (let ((*mixer* (create-mixer :rate 44100)))
    (my-workout)
    (destroy-mixer *mixer*))
  (values))

(defun show-picture ()
  ;; Save as pic.jpg the picture from
  ;; http://graphics8.nytimes.com/images/2013/05/12/health/12well_physed/12well_physed-tmagArticle.jpg
  (run-program (format nil "qiv --center ~A &" (native-namestring (pn "pic.jpg")))))

(defun start ()
  (main-thread-init)
  (mix-it))

(defun main (argv)
  (declare (ignore argv))
  (start))
