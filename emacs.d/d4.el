(require 'cl)

(defun d4-set-background (mode)
  "set background to given mode which is either 'dark or 'light"
  (setq frame-background-mode mode)
  (mapc 'frame-set-background-mode (frame-list))
  (let ((dark  "#000000")
        (light "#FFFFFF"))
    (if (eql mode 'dark)
        (progn (set-background-color dark)
               (set-foreground-color light))
        (progn (set-background-color light)
               (set-foreground-color dark)))))

(defun d4-org-min->string (minutes)
  "convert given minutes to a \"hh:mm\" string
see: d4-org-string->min (inverse)"
  (let* ((hs (* 60 60))
         (ms 60)
         (h (floor (/ minutes ms)))
         (m (mod minutes ms)))
    (format "%d:%02d" h m)))

(defun d4-org-string->min (time)
  "convert given \"hh:mm\" string to minutes
see: d4-org-min->string (inverse)"
  (cl-destructuring-bind (h m)
     (map 'list 'string-to-int
          (split-string time ":"))
     (+ (* 60 h) m)))

(defun d4-org-min-diff (t1 t2)
  "Time difference in minutes between two time strings in \"hh:mm\"
format.
returns a \"hh:mm\" string
see: d4-org-min->string and d4-org-string->min"
  (d4-org-min->string
   (abs (- (d4-org-string->min t1)
           (d4-org-string->min t2)))))

(defun d4-org-sum (&rest args)
  "Sum up all given \"hh:mm\" strings.
returns a \"hh:mm\" string
see: d4-org-min->string and d4-org-string->min"
  (d4-org-min->string
   (reduce '+
           (map 'list 'd4-org-string->min
                args))))

(defun d4-org-avg (&rest args)
  "Average all given \"hh:mm\" strings.
returns a \"hh:mm\" string
see: d4-org-min->string and d4-org-string->min"
  (d4-org-min->string
   (/ (reduce '+
              (map 'list 'd4-org-string->min
                   args))
      (length args))))

(defun d4-clock-into (&optional scope selector)
  "clock into a task inside the given scope (default 'file) by using
selector (fault 'ivy-completing-read) to select it.
see org-map-entries's scope argument for possible scopes
selector gets a list of entries (strings) and should return the entry
to clock into or nil"
  (interactive)
  (let ((scope (or scope 'file))
        (selector (or selector 'ivy-completing-read))
        (entries nil)
        (pos (point))
        (parents nil))
    (flet ((filter (entry-string)
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
                       parents)))
           (add-to-entries ()
             (setf entries
                   (cons (cons (filter (org-current-line-string))
                               (point))
                         entries))))
      ;; TODO: goto 'clock-task-tree' then use 'tree as argument
      (org-map-entries 'add-to-entries nil scope))
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
