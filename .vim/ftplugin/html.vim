if executable("tidy")
        compiler tidy
        setlocal formatprg=tidy\ -i\ -quiet\ --show-errors\ 0
endif
