setlocal ts=4 sw=4 expandtab
if executable("nodelint")
        compiler nodelint
        :autocmd BufWritePost *.js make!
endif
