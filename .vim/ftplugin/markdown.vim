noremap go :call Markdown_preview()<CR>

if executable("markdown")
        compiler markdown
endif
