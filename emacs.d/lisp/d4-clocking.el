;; used to cache results
(defvar d4-last-timestamp nil
  "contains timestamp of last modified org-agenda file")
(defvar d4-last-date nil
  "last date we checked")
(defvar d4-last-result nil
  "cache of last results")

(defun d4-get-current-time ()
  "return minutes since 00:00"
  (abs (floor (- (float-time (org-current-time))
                 (org-time-today))
              60)))

(defun d4-clock-into (&optional scope selector)
  "clock into a task inside the given scope (default 'file) by using
selector (default 'ivy-completing-read) to select it.  see
org-map-entries's scope argument for possible scopes selector gets a
list of entries (strings) and should return the entry to clock into or
nil"
  (interactive)
  (let ((scope (or scope 'file))
        (selector (or selector 'ivy-completing-read))
        (entries nil)
        (pos (point))
        (parents nil))
    (cl-flet ((filter (entry-string)
                (let* ((depth (position #x20 entry-string))
                       (entry (subseq entry-string (+ 1 depth)))
                       (p-depth (length parents)))
                  (cond
                    ((> depth p-depth) (setf parents (cons entry parents)))
                    ((= depth p-depth) (setf (car parents) entry))
                    ((< depth p-depth) (setf parents
                                             (cons entry
                                                   (subseq parents
                                                           (+ 1 (- p-depth depth)))))))
                  (reduce (lambda (new accum)
                            (concat accum "/" new))
                          parents))))
             ;; TODO: goto 'clock-task-tree' then use 'tree as argument
             (org-map-entries (lambda ()
                                (setf entries
                                      (cons (cons (filter (org-current-line-string))
                                                  (point))
                                            entries)))
                              nil scope))
    (let ((entry (find (funcall selector "Clock into: >"
                                (reverse
                                 (mapcar 'car entries)))
                       entries
                       :key 'car
                       :test 'equal)))
      (if entry
          (save-excursion
           (goto-char (cdr entry))
           (org-clock-in))
          (message "no entry found")))))

(defun d4-get-entry-time (entry)
  "returns the time in minutes of a given text entry"
  (let ((time (get-text-property 0 'time-of-day entry)))
    (and time
         (d4-org-strange-time->min time))))

(defun d4-get-entry-duration (entry)
  "returns the duration in minutes of a given text entry"
  (get-text-property 0 'duration entry))

(defun d4-last-change (file)
  "return last-change timestamp of given file"
  (float-time
   (nth 5 (file-attributes file))))

(defun d4-calculate-entries ()
  "return a sorted list (closest first) of all agenda entries of
today, excluding already passed entries.
note: (d4-get-agenda-time-entries) wraps d4-calculate-entries and
caches results if files where not modified"
  (let ((current-time (d4-get-current-time))
        (date (calendar-current-date)))
    (sort
     (delete-if 'null
                (mapcar (lambda (entry)
                          (let ((entry-start-time (d4-get-entry-time entry)))
                            (when (and entry-start-time
                                       (> (+ entry-start-time
                                             (or (d4-get-entry-duration entry)
                                                 (- (* 24 60) entry-start-time)))
                                          current-time))
                              entry)))
                        (apply 'append
                               (mapcar
                                (lambda (agenda-file)
                                  (org-agenda-get-day-entries agenda-file date))
                                org-agenda-files))))
     (lambda (a b)
       (< (d4-get-entry-time a)
          (d4-get-entry-time b))))))

(defun d4-get-agenda-time-entries ()
  "call d4-calculate-entries, but only if files where modified or the
day changed. Cached results will be saved in
d4-last-{timestamp,date,result}"
  (let ((last-modified (apply 'max
                              (mapcar 'd4-last-change
                                      org-agenda-files))))
    (if (and d4-last-timestamp d4-last-date
             (<= last-modified d4-last-timestamp)
             (equalp d4-last-date (calendar-current-date)))
        d4-last-result
        (setq d4-last-timestamp last-modified
              d4-last-date (calendar-current-date)
              d4-last-result (d4-calculate-entries)))))

(defun d4-format-agenda-entry (entry)
  "format given entry to a nice printable string like:
'entry-name' 'when' 'timeframe'
examples:
daily in 1:30 (11:40-12:00)
daily now (11:40-12:00)"
  (let ((ctime (d4-get-current-time))
        (etime (d4-get-entry-time entry)))
    (format "%s %s (%s)"
            (let ((txt (get-text-property 0 'txt entry)))
              (apply 'substring-no-properties txt
                     (when (string-match (org-re "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")
                                         txt)
                       (list 0 (match-beginning 0)))))
            (if (> etime ctime)
                (format "in %s"
                        (d4-org-min->string
                         (- etime ctime)))
                "now")
            (get-text-property 0 'time entry))))

(defun d4-upcoming-entries ()
  "nicely formatted string with displays upcoming org agenda entries"
  (let* ((ctime (d4-get-current-time))
         (entries (d4-get-agenda-time-entries))
         (upcoming nil))
    (cl-loop for entry in entries
             if (let* ((etime-start (d4-get-entry-time entry))
                       (etime-end (+ etime-start
                                     (or (d4-get-entry-duration entry) 0))))
                  (or (and (<= etime-start ctime)
                           (>= etime-end ctime))
                      (and (null upcoming)
                           (> etime-start ctime))))
             do (push entry upcoming))
    (when upcoming
      (concat "["
              (reduce (lambda (accum &optional new)
                        (if new
                            (concat accum ", " new)
                            accum))
                      (mapcar 'd4-format-agenda-entry
                              (reverse upcoming)))
              "]"))))

(provide 'd4-clocking)
