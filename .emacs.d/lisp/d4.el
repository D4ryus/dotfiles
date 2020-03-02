(require 'cl)
(require 'org-agenda)

(require 'd4-org)

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

(defun d4-buffer-mode (buffer-or-name)
  "return mode of given buffer"
  (with-current-buffer buffer-or-name major-mode))

(defun d4-filter-buffers-by-mode (mode &optional buffer-list)
  "return all buffers with given mode"
  (delq nil
        (mapcar (lambda (buffer)
                  (and (eq (d4-buffer-mode buffer) mode) buffer))
                (or buffer-list (buffer-list)))))

(defun d4-set-style (style &rest modes)
  "set style to all buffers with given modes"
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (c-set-style style)))
        (apply 'append (mapcar 'd4-filter-buffers-by-mode modes))))

(defun d4-to-bin (number)
  (apply 'concat
         (reverse
          (loop for i to 31
                collect
                (format
                 (if (and (not (eql i 0))
                          (eql (mod i 4) 0))
                     "%d "
                     "%d")
                 (logand #x1 (lsh number (- i))))))))

(defun d4-to-ascii (number)
  (apply 'concat
         (reverse
          (loop for i to 7
                collect
                (format
                 "%c"
                 (c-int-to-char
                  (logand #xff (lsh number (- (* i 8))))))))))

(defun d4-YYY ()
  (interactive)
  (insert
   "#define YYY(__fmt, ...)					/* YYY */\\\n"
   "	do {							/* YYY */\\\n"
   "		printf(\"YYY:%s:%s:%d:\" __fmt \"\\n\",		/* YYY */\\\n"
   "		    __FILE__, __PRETTY_FUNCTION__, __LINE__,	/* YYY */\\\n"
   "		    ##__VA_ARGS__);				/* YYY */\\\n"
   "	} while (0)						/* YYY */\n"))

(provide 'd4)
