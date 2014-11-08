setlocal makeprg=javac\ %
setlocal efm=%A%f:%l:\ error:\ %m,%Z%.%#,%-G%.%#
