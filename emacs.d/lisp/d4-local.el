(add-hook 'term-setup-hook
          '(lambda ()
             (define-key function-key-map "\e[1;5A" [C-up])
             (define-key function-key-map "\e[1;5B" [C-down])
             (define-key function-key-map "\e[1;5C" [C-right])
             (define-key function-key-map "\e[1;5D" [C-left])))

(setq slime-inhibit-pipelining nil)

(setq reftex-default-bibliography '("~/.dotfiles/bibliothek.bib"))

;; see org-ref for use of these variables
(setq ;; org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/.dotfiles/bibliothek.bib"))

(setq org-ref-ivy-cite t)

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"makeglossaries %b"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-classes
             (cdr (assoc "scrreprt" org-latex-classes))
             '("scrreprt" "\\documentclass[11pt]{scrreprt}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(c-add-style "user"
             '("bsd"
               (c-basic-offset . 8)
               (tab-width . 8)
               (fill-column . 80)
               (indent-tabs-mode t)
               (c-comment-only-line-offset . 0)
               ;; hit C-c C-s on line to get variable
               (c-offsets-alist (label . 0)
                (inline-open . 0)
                (inexpr-class . 0)
                (substatement-label . 0)
                (arglist-intro . *)
                (statement-cont . *)
                (func-decl-cont . *)
                (substatement-open . *)
                (member-init-intro . *)
                (arglist-cont-nonempty . *)
                (func-decl-cont . +)
                (statement-block-intro . +))))

(setq c-default-style "user")

(mapc 'find-file
      (list "~/.emacs.d/init.el"
            "~/.emacs.d/lisp"))

(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

(add-to-list 'initial-frame-alist
             '(font . "Noto Mono-10:antialias=subpixel"))

(provide 'd4-local)
