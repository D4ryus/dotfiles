" vim:ts=8:sw=8:foldmethod=marker:

" Plug {{{1

" auto-install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
        silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-git'
Plug 'tpope/vim-eunuch'
Plug 'kien/ctrlp.vim'

Plug 'kien/rainbow_parentheses.vim', {'on': 'RainbowParenthesesToggle'}
Plug 'dhruvasagar/vim-table-mode',   {'on': 'TableModeEnable'}
Plug 'tpope/vim-dispatch',           {'on': 'Make'}
Plug 'godlygeek/tabular',            {'on': 'Tabularize'}
Plug 'majutsushi/tagbar',            {'on': 'TagbarToggle'}
Plug 'gregsexton/gitv',              {'on': 'Gitv'}

Plug 'tpope/vim-fireplace', {'for': 'clojure'}
Plug 'vim-scripts/paredit.vim', {'for': 'clojure'}

if executable('ctags')
        Plug 'ludovicchabant/vim-gutentags'
endif

if has("python")
        Plug 'kovisoft/slimv', {'for': 'lisp'}
endif

if has("python") || has("python3")
        Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}
endif

if has("python3")
        Plug 'SirVer/ultisnips'
endif

call plug#end()

" Plug }}}1
" plugin-settings {{{1

" UltiSnips {{{2

let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"
let g:UltiSnipsEditSplit = "vertical"
let g:UltiSnipsSnippetsDir = "~/.vim/UltiSnips"

" UltiSnips }}}2
" Eclim {{{2

let g:EclimLoggingDisabled = 1
let g:EclimCompletionMethod = 'omnifunc'
let g:EclimXmlValidate = 0

command! Jio JavaImportOrganize
command! Jdp JavaDocPreview
command! Jch JavaCallHierarchy
command! Jc  JavaCorrect
command! Jr  JavaRename
command! Jgs JavaGetSet
command! Jg  JavaGet
command! Js  JavaSet

" Eclim }}}2
" Tagbar {{{2

let Tlist_Use_Right_Window = 1
let Tlist_File_Fold_Auto_Close = 1

noremap cot :TagbarToggle<CR>

" Tagbar }}}2
" Ag {{{2

if executable('ag')
        set grepprg=ag\ --vimgrep
endif

" Ag }}}2
" GitV {{{2

let g:Gitv_OpenPreviewOnLaunch = 1
let g:Gitv_DoNotMapCtrlKey = 1

" GitV }}}2
" Gundo {{{2

if has("python3")
        let g:gundo_prefer_python3 = 1
endif
noremap cog :GundoToggle<CR>

" Gundo }}}2
" Slimv {{{2

let g:slimv_repl_split = 4
noremap cop :RainbowParenthesesToggle<CR>

if has('nvim')
        let g:slimv_swank_cmd = ':sp term://sbcl'
              \ . ' --dynamic-space-size 4096'
              \ . ' --load ~/.vim/plugged/slimv/slime/start-swank.lisp'
              \ . ' | :hide'
endif

" Slimv }}}2
" Paredit {{{2

let g:paredit_mode = 1

" Paredit }}}2
" Fireplace {{{2

noremap ,e :silent! Eval<CR>:Last!<CR>

" Fireplace }}}2

" plugin-settings }}}1
