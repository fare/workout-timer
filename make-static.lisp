#!/bin/sh
":" ; exec cl -Q -sp asdf "$0" ${1:-"$@"}

(make "workout-timer/static")
