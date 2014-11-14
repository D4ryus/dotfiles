if executable("php")
        compiler php
        :autocmd BufWritePost *.php make!
endif
