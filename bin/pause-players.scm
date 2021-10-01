#! /bin/sh
# -*- mode: scheme; coding: utf-8 -*-
exec guile -e main -s "$0" "$@"
!#
(use-modules (ice-9 format)
             (dbus))

(define (main . args)
  (with! (bus (make <bus>))
    (for-each
     (lambda (name)
       (when (string-prefix? "org.mpris.MediaPlayer2." name)
         (false-if-exception
          (invoke-method bus "/org/mpris/MediaPlayer2" "Pause"
                         #:destination name
                         #:interface "org.mpris.MediaPlayer2.Player"))))
     (array->list (list-names bus)))))
