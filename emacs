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
(package-install 'ac-c-headers)

(evil-mode 1)

;; helm
(require 'helm-config)

;; jk as esc on evil
(require 'key-chord)
(key-chord-mode +1)
(key-chord-define-global "jk" 'evil-normal-state)

(global-set-key (kbd "M-x") 'execute-extended-command)

;; ctrl-p
(define-key evil-normal-state-map (kbd "C-p") 'helm-find)
(define-key evil-insert-state-map (kbd "C-p") 'helm-find)
(global-set-key (kbd "C-p") 'helm-find)

(custom-set-variables
        ;; custom-set-variables was added by Custom.
        ;; If you edit it by hand, you could mess it up, so be careful.
        ;; Your init file should contain only one such instance.
        ;; If there is more than one, they won't work right.
        '(ansi-color-faces-vector
          [default default default italic underline success warning error])
        '(custom-enabled-themes (quote (tango-dark)))
        '(font-use-system-font t)
        '(global-auto-complete-mode t)
        '(inhibit-startup-screen t)
        '(menu-bar-mode t)
        '(show-paren-mode t)
        '(show-trailing-whitespace t)
        '(tool-bar-mode nil))
(custom-set-faces
        ;; custom-set-faces was added by Custom.
        ;; If you edit it by hand, you could mess it up, so be careful.
        ;; Your init file should contain only one such instance.
        ;; If there is more than one, they won't work right.
        '(default ( (t (:inherit nil
                        :stipple nil
                        :background "#2e3436"
                        :foreground "#eeeeec"
                        :inverse-video nil
                        :box nil
                        :strike-through nil
                        :overline nil
                        :underline nil
                        :slant normal
                        :weight normal
                        :height 111
                        :width normal
                        :foundry "bitstream"
                        :family "Terminus")))))

(setq c-default-style "bsd"
        c-basic-offset 8
        tab-width 8
        indent-tabs-mode t)
