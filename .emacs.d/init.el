(package-initialize)

(setq custom-file
      (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar local-lisp-directory
  (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path local-lisp-directory)

(require 'd4-pkg)
(require 'd4-org)
(when (file-exists-p (concat local-lisp-directory "d4-local.el"))
  (require 'd4-local))
(require 'd4-clocking)
;; --- custom functions

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

(defun jarvis-copy (&optional text)
  (interactive)
  (dbus-call-method
   :session "net.d4ryus.Jarvis"
   "/net/d4ryus/Jarvis"
   "net.d4ryus.Clipboard" "Copy"
   (or text
       (and (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end)))
       (error "Copy requires region or text"))))

(defun jarvis-kill ()
  (interactive)
  (let ((text
          (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
              (error "Select text to kill"))))
    (delete-active-region t)
    (jarvis-copy text)))

(defun jarvis-paste ()
  (interactive)
  (let ((text (dbus-call-method
               :session "net.d4ryus.Jarvis"
               "/net/d4ryus/Jarvis"
               "net.d4ryus.Clipboard" "Paste")))
    (when (region-active-p)
      (delete-active-region t))
    (insert text)))


;; backup settings
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backup/")))

;; cache settings
(setq auto-save-file-name-transforms
      '((".*" "~/.cache/emacs/" t)))

;; dont show tool-bar
(custom-set-variables
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))
;; auto revert buffers
(global-auto-revert-mode)
;; show current function in modeline
(which-function-mode)
;; always show matching parens
(show-paren-mode)

;; dont show scroll-bar and set font
(setq default-frame-alist
      '((font . "JetBrains Mono 10")
        (vertical-scroll-bars . nil)))

;; alias st and screen (tmux) to xterm
(mapc (lambda (term)
        (add-to-list 'term-file-aliases
                     (cons term "xterm-256color")))
      '("st-256color"
        "screen-256color"
        "rxvt-unicode-256color"))

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x g")
  (lambda ()
    (interactive)
    (grep (format "grep -nH '\\<%s\\>' -R ."
                  (thing-at-point 'symbol)))))

(setq-default
 ;; disable startup message
 inhibit-startup-message t
 ;; indent with spaces per default
 indent-tabs-mode nil
 ;; show column number on mode line
 column-number-mode t
 ;; show empty lines
 indicate-empty-lines t
 ;; dont break words by wrapping to new line
 word-wrap t
 ;; org-agenda dont show holidays
 org-agenda-include-diary nil
 org-tags-column 80
 org-return-follows-link t
 ;; add timestamp to done tasks
 org-log-done 'time
 ;; use drawer (LOGBOOK)
 org-log-into-drawer t
 org-clock-into-drawer t
 ;; Align text with headline
 org-adapt-indentation t
 org-hide-leading-stars nil
 org-ellipsis "⮷"
 ring-bell-function 'ignore
 ;; grep recursive inside current directory
 grep-command "grep -nH -R . -e "
 ;; scroll single lines when cursor moves out of window
 scroll-conservatively 101
 ;; Disable vc-mode on tramp buffers
 vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                              vc-ignore-dir-regexp
                              tramp-file-name-regexp))

(defun d4-recenter-bottom-hook (frame)
  (when (eql (point) (point-max))
    (recenter (window-body-height))))

(add-to-list 'window-size-change-functions
             'd4-recenter-bottom-hook)

;; copied from /r/emacs
(defun d4-backup-scratch ()
  (with-current-buffer "*scratch*"
    (when (> (buffer-size)
             (length (substitute-command-keys initial-scratch-message)))
      (let ((dir (concat user-emacs-directory "scratch/")))
        (make-directory dir t)
        (write-file (expand-file-name
                     (format-time-string "scratch-%F.%T.el")
                     dir))))))

(add-hook 'kill-emacs-hook #'d4-backup-scratch)

;; default c coding styles and settings
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "bsd"
                  c-basic-offset 8
                  tab-width 8
                  fill-column 80
                  indent-tabs-mode t
                  cc-search-directories (append cc-search-directories
                                                '("../*"
                                                  "../../*"
                                                  "../../../*")))))

;; auto update proced
(add-hook 'proced-mode-hook
          (lambda ()
            (proced-toggle-auto-update t)))
