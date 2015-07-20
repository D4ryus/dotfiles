(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(package-initialize)
(package-install 'evil)
(package-install 'key-chord)

(require 'evil)
(evil-mode 1)

(require 'key-chord)
(key-chord-mode +1)
(key-chord-define-global "jk" 'evil-normal-state)

(tool-bar-mode -1)

(setq c-default-style "bsd"
      c-basic-offset 8
      tab-width 8
      indent-tabs-mode t)
