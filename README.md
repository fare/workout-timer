workout-timer
=============

`workout-timer` is a trivial workout timer written in Common Lisp.
Its behavior can be configured with various invocation flags,
but its default mode is to offer a
[7-minute workout](http://well.blogs.nytimes.com/2013/05/09/the-scientific-7-minute-workout/).
Being written in Common Lisp, it is pretty easy to modify
for anyone who knows how to program in Lisp.

`workout-timer` has only been tested on Linux
(using SBCL, but any other implementation should do).
All its dependencies are in [Quicklisp](https://www.quicklisp.org/beta/).
Its main dependency is [mixalot](http://cliki.net/mixalot)
which itself depends on various C libraries
that may or may not configuration tweaks to work on other operating systems.
Its invocation script relies on [cl-launch](http://cliki.net/cl-launch).

On Ubuntu, I notably had to:

    apt-get install sbcl cl-launch libvorbis-dev


Invocation
----------

From a Unix shell command-line, you can invoke it with:

    ./workout-timer

Get its version with:

    ./workout-timer -V

Usage with:

    ./workout-timer -h

Implicit default behavior:

    ./workout-timer -v 1.0 -w 30 -p 10 \
      -e "Jumping jack" -e "Wall sit" -e "Push-up" -e "Abdominal crunch" \
      -e "Step-up onto chair" -e "Squat" -e "Triceps dip on chair" -e "Plank" \
      -e "High knees running in place" -e "Lunge" -e "Push-up and rotation" -e "Side plank"

Or, from the SBCL (SLIME) REPL:

    (load "~/quicklisp/setup.lisp")
    (ql:quickload "workout-timer")
    (workout-timer:mix-it :volume 1.0 :work-seconds 30 :pause-seconds 10)
