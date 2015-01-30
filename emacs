(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	                  '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(package-install 'evil)
(package-install 'helm)
(package-install 'key-chord)

(evil-mode 1)

;; jk as esc on evil
(require 'key-chord)
(key-chord-mode +1)
(key-chord-define-global "jk" 'evil-normal-state)

;; M-x binding in insertmode
(global-set-key (kbd "M-x") 'smex)
(define-key evil-insert-state-map (kbd "M-x") 'execute-extended-command)
(define-key evil-normal-state-map (kbd "M-x") 'execute-extended-command)

;; helm
(require 'helm-config)

(custom-set-variables
        ;; custom-set-variables was added by Custom.
        ;; If you edit it by hand, you could mess it up, so be careful.
        ;; Your init file should contain only one such instance.
        ;; If there is more than one, they won't work right.
        '(ansi-color-faces-vector
	         [default default default italic underline success warning error])
               '(custom-enabled-themes (quote (tango-dark)))
	              '(inhibit-startup-screen t)
		             '(menu-bar-mode t)
			            '(tool-bar-mode nil))
(custom-set-faces
        ;; custom-set-faces was added by Custom.
        ;; If you edit it by hand, you could mess it up, so be careful.
        ;; Your init file should contain only one such instance.
        ;; If there is more than one, they won't work right.
        )
