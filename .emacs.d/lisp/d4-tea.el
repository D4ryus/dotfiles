(require 'notifications)

(defvar d4-tea-timer nil)
(defvar d4-tea-cnt 0)

(defun d4-tea-close-cb (id reason)
  (message "Dude, TEA!")
  (if (eql reason 'dismissed)
      (setq d4-tea-cnt 0)
      (d4-notify-tea)))

(defun d4-notify-tea ()
  (notifications-notify :title (concat "Tea"
                                       (make-string (incf d4-tea-cnt) ?\!))
                        :body "dude"
                        :timeout 1000
                        :ugrency 'critical
                        :on-close 'd4-tea-close-cb))

(defun d4-tea ()
  (interactive)
  (when (or (not d4-tea-timer)
            (when (y-or-n-p "Timer Already running, cancel?")
              (prog1 t
                (cancel-timer d4-tea-timer)
                (setq d4-tea-timer nil))))
    (let ((time (read-from-minibuffer "Time: " "4 min")))
      (setq d4-tea-timer
            (run-at-time time nil
                         (lambda ()
                           (d4-notify-tea)
                           (setq d4-tea-timer nil)))))))

(provide 'd4-tea)
