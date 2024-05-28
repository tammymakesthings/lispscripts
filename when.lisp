(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:adopt :iterate :chronicity :local-time :losh)
                :silent t))

(defpackage :when
  (:use :cl :iterate)
  (:export :toplevel :*ui*))

(in-package :when)

;;;; Configuration ------------------------------------------------------------
(defparameter +iso-8601-seconds-format+
  '((:year 4) #\- (:month 2) #\- (:day 2)
    #\T
    (:hour 2) #\: (:min 2) #\: (:sec 2)
    :gmt-offset-or-z))


(defparameter *time-formats*
  `((:rfc-3339 . ,local-time:+rfc3339-format+)
    (:iso-8601 . ,local-time:+iso-8601-format+)
    (:iso-8601-seconds . ,+iso-8601-seconds-format+)
    (:gnuplot . ((:day 2) #\/ (:month 2) #\/ (:year 4) #\, (:hour 2) #\: (:min 2)))))


;;;; Errors -------------------------------------------------------------------
(define-condition user-error (error) ())

(define-condition need-one-time-string (user-error) ()
  (:report "Exactly one time string is required."))



;;;; Functionality ------------------------------------------------------------
(defun jitter-ns (seconds)
  (if (zerop seconds)
    0
    (_ (coerce seconds 'double-float)
      random
      (* 1000000000 _)
      floor)))

(defun run (time-string &key (output-format :iso-8601) (jitter 0) timezone)
  (local-time:format-timestring
    *standard-output*
    (local-time:timestamp+ (chronicity:parse time-string :endian-preference :middle)
                           (jitter-ns jitter)
                           :nsec)
    :format (or (assocdr output-format *time-formats*)
                (error "Unknown time format ~S." output-format))
    :timezone (or timezone local-time:*default-timezone*)))


;;;; User Interface -----------------------------------------------------------
(defparameter *examples*
  '(("Print the current time in RFC 3339 format:" . "when now --rfc-3339")
    ("Print a randomish time from a while ago:" . "when '12 hours ago' --jitter-hours 4")
    ("Print a randomish time next year in UTC:" . "when '1 year from now' --jitter-days 30 --utc")))


(defparameter *option/help*
  (adopt:make-option 'help
    :help "Display help and exit."
    :long "help"
    :short #\h
    :reduce (constantly t)))


(defparameter *option/output-format/iso-8601*
  (adopt:make-option 'output-format/iso-8601
    :result-key 'output-format
    :help "Output results as ISO-8601 (default)."
    :long "iso-8601"
    :short #\i
    :initial-value :iso-8601
    :reduce (constantly :iso-8601)))

(defparameter *option/output-format/iso-8601-seconds*
  (adopt:make-option 'output-format/iso-8601-seconds
    :result-key 'output-format
    :help "Output results as ISO-8601 with a precision of seconds (no nanoseconds)."
    :long "iso-8601-seconds"
    :short #\I
    :reduce (constantly :iso-8601-seconds)))

(defparameter *option/output-format/rfc-3339*
  (adopt:make-option 'output-format/rfc-3339
    :result-key 'output-format
    :help "Output results as RFC 3339."
    :long "rfc-3339"
    :short #\r
    :reduce (constantly :rfc-3339)))

(defparameter *option/output-format/gnuplot*
  (adopt:make-option 'output-format/gnuplot
    :result-key 'output-format
    :help "Output results as Gnuplot timestamps."
    :long "gnuplot"
    :short #\g
    :reduce (constantly :gnuplot)))


(defparameter *option/jitter/seconds*
  (adopt:make-option 'jitter/seconds
    :result-key 'jitter
    :parameter "N"
    :help "Jitter result by up to N seconds."
    :long "jitter-seconds"
    :short #\S
    :initial-value 0
    :key #'parse-integer
    :reduce (lambda (old n) (declare (ignore old)) n)))

(defparameter *option/jitter/minutes*
  (adopt:make-option 'jitter/minutes
    :result-key 'jitter
    :parameter "N"
    :help "Jitter result by up to N minutes."
    :long "jitter-minutes"
    :short #\M
    :key #'parse-integer
    :reduce (lambda (old n) (declare (ignore old)) (* 60 n))))

(defparameter *option/jitter/hours*
  (adopt:make-option 'jitter/hours
    :result-key 'jitter
    :parameter "N"
    :help "Jitter result by up to N hours."
    :long "jitter-hours"
    :short #\H
    :key #'parse-integer
    :reduce (lambda (old n) (declare (ignore old)) (* 60 60 n))))

(defparameter *option/jitter/days*
  (adopt:make-option 'jitter/days
    :result-key 'jitter
    :parameter "N"
    :help "Jitter result by up to N days."
    :long "jitter-days"
    :short #\D
    :key #'parse-integer
    :reduce (lambda (old n) (declare (ignore old)) (* 24 60 60 n))))


(defparameter *option/timezone/local*
  (adopt:make-option 'jitter/timezone/local
    :result-key 'timezone
    :help "Print result in current local computer timezone (default)."
    :long "local-timezone"
    :short #\l
    :initial-value nil
    :reduce (constantly nil)))

(defparameter *option/timezone/utc*
  (adopt:make-option 'jitter/timezone/utc*
    :result-key 'timezone
    :help "Print the result in UTC."
    :long "utc"
    :short #\u
    :reduce (constantly local-time:+utc-zone+)))


(adopt:define-string *help-text*
  "when takes a human-readable time like '2 hours ago' and prints it as a ~
  computer-readable time.  It can also jitter the time by a random amount if ~
  you want to produce randomish data.")


(defparameter *ui*
  (adopt:make-interface
    :name "when"
    :usage "[OPTIONS] TIME-STRING"
    :summary "convert human time to computer time"
    :help *help-text*
    :examples *examples*
    :contents (list *option/help*
                    (adopt:make-group
                      'output-options
                      :title "Output Options"
                      :options (list *option/output-format/iso-8601*
                                     *option/output-format/iso-8601-seconds*
                                     *option/output-format/rfc-3339*
                                     *option/output-format/gnuplot*))
                    (adopt:make-group
                      'timezone-options
                      :title "Timezone Options"
                      :options (list *option/timezone/local*
                                     *option/timezone/utc*))
                    (adopt:make-group
                      'jitter-options
                      :title "Jitter Options"
                      :options (list *option/jitter/seconds*
                                     *option/jitter/minutes*
                                     *option/jitter/hours*
                                     *option/jitter/days*)))))


(defun toplevel ()
  (sb-ext:disable-debugger)
  (setf *random-state* (make-random-state t))
  (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
    (handler-case
        (adopt::quit-on-ctrl-c ()
          (cond
            ((gethash 'help options) (adopt:print-help-and-exit *ui*))
            ((/= (length arguments) 1) (error 'need-one-time-string))
            (t (run (first arguments)
                    :output-format (gethash 'output-format options)
                    :jitter (gethash 'jitter options)
                    :timezone (gethash 'timezone options)))))
      (user-error (e) (adopt:print-error-and-exit e)))))
