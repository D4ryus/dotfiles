((emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (eval . (put 'cl-destructuring-bind 'lisp-indent-function 2))
                     (eval . (put 'cl-destructuring-bind 'common-lisp-indent-function 2))
                     (eval . (put 'use-package 'lisp-indent-function 1))
                     (eval . (put 'use-package 'common-lisp-indent-function 1)))))

