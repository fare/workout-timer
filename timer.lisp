(uiop:define-package :workout-timer/timer
  (:nicknames :workout-timer)
  (:use :common-lisp :uiop
	:mixalot :mixalot-vorbis :vorbisfile
	:local-time :command-line-arguments)
  (:export #:main #:mix-it #:start-mixer #:end-mixer))

(in-package :workout-timer/timer)

(defparameter *version* "1.0.3")

(defparameter *mixer* nil)

(defparameter *current-time* nil)

(defparameter +workout-timer-option-spec+
  '((("work-seconds" #\w) :type integer :optional t :documentation "duration of each exercise, in seconds")
    (("pause-seconds" #\p) :type integer :optional t :documentation "duration of pause between exercises, in seconds")
    (("exercise" #\e) :action :exercises :type string :list t :optional t :documentation "name of an exercise (repeat option as needed)")
    (("volume" #\v) :type string :optional t :documentation "volume multiplier")
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

(defun get-vorbis-length-rate (x)
  (let* ((filename (oggfile x))
	 (handle (vorbis-new)))
    (unwind-protect
	 (progn
	   (vorbis-open filename handle)
	   (values (get-vorbis-length handle) (get-vorbis-rate handle)))
      (vorbis-close handle))))

(defun get-vorbis-sample (x &key (mixer *mixer*) volume)
  (let* ((streamer (make-vorbis-streamer (oggfile x)))
	 (length (streamer-length streamer mixer))
	 (rate (vorbis-sample-rate streamer))
	 (sample (make-array length :element-type 'stereo-sample)))
    (streamer-write-into streamer mixer sample 0 length 0)
    (unless (member volume '(nil 1 1.0d0))
      (loop :for i :from 0 :for s :across sample :do
	(setf (aref sample i)
	      (stereo-sample (round (* (stereo-left s) volume))
			     (round (* (stereo-right s) volume))))))
    (streamer-cleanup streamer mixer)
    (values sample rate)))

;; Samples from the respective URLs, as recoded with sox ${in} -r 44100 ${out}.ogg
(defvar *bell* nil) ;; https://www.freesound.org/people/Taira%20Komori/sounds/212057/
(defvar *click* nil) ;; https://www.freesound.org/people/EdgardEdition/sounds/113634/
(defvar *buzzer* nil) ;; https://www.freesound.org/people/guitarguy1985/sounds/54047/
(defvar *gong* nil) ;; https://www.freesound.org/people/Veiler/sounds/207168/

(defun get-samples (&optional volume)
  (multiple-value-setq (*bell* *click* *buzzer* *gong*)
    (apply 'values (mapcar (lambda (x) (get-vorbis-sample x :volume volume))
			   '("bell" "click" "buzzer" "gong")))))

(defun add-sample (sample)
  (mixer-add-streamer *mixer* (make-fast-vector-streamer-joint-stereo sample)))

(defun bell () (add-sample *bell*))
(defun click () (add-sample *click*))
(defun buzzer () (add-sample *buzzer*))
(defun gong () (add-sample *gong*))

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

(defun start-mixer ()
  (unless *mixer*
    (main-thread-init) ;; NB: mixalot is well-designed enough not to do it twice
    (setf *mixer* (create-mixer :rate 44100)))
  *mixer*)

(defun end-mixer ()
  (when *mixer*
    (destroy-mixer *mixer*)
    (setf *mixer* nil))
  (values))

(defun mix-it (&key work-seconds pause-seconds exercises volume)
  (start-mixer)
  (get-samples volume)
  (my-workout
   :work-seconds work-seconds
   :pause-seconds pause-seconds
   :exercises exercises)
  (end-mixer))

(defun show-picture ()
  ;; Save as pic.jpg the picture from
  ;; http://graphics8.nytimes.com/images/2013/05/12/health/12well_physed/12well_physed-tmagArticle.jpg
  (run-program (format nil "qiv --center ~A &" (native-namestring (pn "pic.jpg")))))

(defun parse-double-float (string)
  "Parse STRING as a DOUBLE-FLOAT number."
  (flet ((bad () (error "Not a valid floating-point number: ~A" string)))
    (unless (every (lambda (x) (find x "-.0123456789e")) string) (bad))
    (with-input-from-string (s string)
      (let ((object
	     (let ((*read-default-float-format* 'double-float))
	       (ignore-errors
		 (read s)))))
	(unless (realp object) (bad))
	(when (peek-char nil s nil) (bad))
	(coerce object 'double-float))))) ;; object might be an integer

(defun workout-timer (&key work-seconds pause-seconds exercises volume help version)
  (cond
    (version (show-version))
    (help (show-help))
    (t (mix-it :work-seconds work-seconds
	       :pause-seconds pause-seconds
	       :exercises exercises
	       :volume (when volume (parse-double-float volume))))))
