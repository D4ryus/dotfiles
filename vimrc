" file: ~/.vimrc
" author: d4ryus - https://github.com/d4ryus/
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

if v:version >= 800
        Plug 'maralla/completor.vim'
endif

if executable('ctags')
        Plug 'ludovicchabant/vim-gutentags'
endif

if has("python")
        Plug 'kovisoft/slimv', {'for': 'lisp'}
endif

if has("python") || has("python3")
        Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}
endif

if v:version >= 704 && has("python") || has("python3")
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
" setter {{{1

if !has('nvim')
        set ttyfast             " assume a fast terminal
        set nocompatible        " set noncompatible mode (vi vim)
endif

syntax on                       " enable syntax highlighting
set showcmd                     " display incomplete commands
set incsearch                   " do incremental searching
set ignorecase                  " dont use case sensetive search
set autoread                    " autoread file when changed from outside
set smartindent                 " should work better than autoindent
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set list                        " list all tabs and ending spaces
set listchars=tab:>_,nbsp:_,trail:_,precedes:<,extends:>,eol:¬
set linebreak                   " better wraping of lines
set showbreak=>>                " show linebreaks if wrap is set
set expandtab                   " use spaces instead of tabs
set tabstop=8                   " amout of spaces per tab
set shiftwidth=8                " number of spaces used by autoindent
set history=82                  " keep 82 lines of command line history
set backspace=indent,eol,start  " allow backspace in insert mode
set encoding=UTF-8              " use UTF-8 as encoding
set modelines=40                " search first/last 40 lines for vim modeline
set laststatus=2                " allways show statusline
set spelllang=en,de             " set spelling language to english and german
set directory=~/.vim/swap//     " directory where all swap files will be
set foldtext=NeatFoldText()     " set foldtext to function below
set completeopt=menuone         " show menu if only 1 match
set completeopt+=preview        " enable preview window
set completeopt+=noinsert       " do not insert anything
set completeopt+=noselect       " do not select a match, just open suggestions
set splitright                  " open splits on the right side instead of left
set splitbelow                  " open splits blow instead of on top
set wildmenu                    " use wildmenu
set wildmode=longest:list,full  " dont change insertion
set path+=/usr/local/include    " also search through /usr/local/include
set path+=.,**                  " also add the current path
set diffopt+=vertical           " vertical diff
set nojoinspaces                " do not add spaces on join
set formatoptions-=o            " do not continue comment after hitting 'o'
set wildignore+=*.o,*.class,*.so " ignore binary files
set wildignore+=*/.git/**/*     " ignore git files
set wildignore+=tags            " ignore tags file
set display=lastline            " show as much wrapped lines as possible
set mouse=                      " disable mouse

" move cursor everywhere in visual block mode
if has('virtualedit')
        set virtualedit+=block
endif

" use blowfish as encryption (:X)
if !has('nvim')
        if v:version >= 704
                set cm=blowfish2
        elseif v:version >= 702
                set cm=blowfish
        endif
        set clipboard=unnamed,unnamedplus
else
        set clipboard=unnamed
endif

" indent linebreaks
if v:version == 704 && has('patch338')
        set breakindent
endif

if executable('par')
        set formatprg=par
endif

if !isdirectory($HOME . '/.vim/swap') && exists('*mkdir')
        call mkdir($HOME . '/.vim/swap', 'p', 0700)
endif

" setter }}}1
" mappings {{{1

" noremap {{{2

noremap zv zMzv
noremap tab :Tabularize /
noremap go :Make<CR>

nnoremap gb :ls<CR>:b<Space>
nnoremap coo :/\%80v.\+/ 

" swap k/j <-> gk/gj
nnoremap k gk
nnoremap gk k
nnoremap j gj
nnoremap gj j

" noremap }}}2
" inoremap {{{2

inoremap jk <Esc>

" inoremap }}}2
" vnoremap {{{2

vnoremap < <gv
vnoremap > >gv

" vnoremap }}}2
" cnoremap {{{2

cnoremap <expr> %% getcmdtype() == ':' ? expand('%h').'/' : '%%'

" cnoremap}}}2
" tnoremap {{{2

if has('nvim')
        tnoremap <Esc><Esc> <C-\><C-n>
endif

" tnoremap}}}2

" mappings }}}1
" abbrev {{{1

iabbrev date- <c-r>=strftime("%Y-%m-%d")<CR>
iabbrev file- <c-r>%<CR>

" abbrev }}}1
" appearance {{{1

" background {{{2

set background=dark

" background }}}2
" statusline {{{2

set statusline=%<[%F%M%R]                           "[filename(,Flags)]
set statusline+=\ [%{FileSize()}\|                  "[Filesize()|
set statusline+=\%{&ff}\|                           "FileFormat|
set statusline+=\%{''.(&fenc!=''?&fenc:&enc).''}\|  "Encoding|
set statusline+=\%Y]                                "FileType]
set statusline+=\ %{fugitive#statusline()}%=        "Fugitive
set statusline+=\ [pos:\ %l/%L:%v\ %p%%]            "[pos: 1/2:3 50%]

" statusline }}}2
" matchparen {{{2

highlight MatchParen ctermbg=0 ctermfg=1 cterm=bold
highlight SpecialKey ctermfg=234

" matchparen }}}2
" diff {{{2

highlight DiffDelete ctermbg=1 ctermfg=0 cterm=none guibg=Red    guifg=Black gui=none
highlight DiffAdd    ctermbg=2 ctermfg=0 cterm=none guibg=Green  guifg=Black gui=none
highlight DiffChange ctermbg=3 ctermfg=0 cterm=none guibg=Yellow guifg=Black gui=none
highlight DiffText   ctermbg=4 ctermfg=7 cterm=none guibg=Blue   guifg=White gui=none

" diff }}}2

" appearance }}}1
" functions {{{1

" FileSize {{{2

"set statusline=%{FileSize()}
function! FileSize()
        let bytes = getfsize(expand("%:p"))
        if bytes <= 0
                return ""
        endif
        if bytes < 1024
                return bytes
        else
                return (bytes / 1024) . "K"
        endif
endfunction

" FileSize }}}2
" NeatFoldText {{{2

function! NeatFoldText()
" got this function from http://dhruvasagar.com/tag/vim thanks alot :)
        let line             = ' ' . substitute(getline(v:foldstart),
                                \ '^\s*"\?\s*\|\s*"\?\s*{{'
                                \ . '{\d*\s*', '', 'g')
                                \ . ' '
        let lines_count      = v:foldend - v:foldstart + 1
        let lines_count_text = '| ' . printf("%9s", lines_count . ' lines')
                                \ . ' |'
        "let foldchar         = matchstr(&fillchars, 'fold:\zs.')
        let foldchar         = '_'
        let foldtextstart    = strpart('+' . repeat(foldchar, v:foldlevel)
                                \ . line, 0, (winwidth(0) * 2) / 3)
        let foldtextend      = lines_count_text . repeat(foldchar, 8)
        let foldtextlength   = strlen(substitute(foldtextstart . foldtextend,
                                \ '.', 'x', 'g')) + &foldcolumn
        return foldtextstart . repeat(foldchar, winwidth(0) - foldtextlength)
                                \ . foldtextend
endfunction

" NeatFoldText }}}2
" NumberFold {{{2
"
function! NumberFold()
        let h = matchstr(getline(v:lnum), '^\(\d\+.\)\+')
        if empty(h)
                return "="
        else
                return ">" . (len(h) / 2)
        endif
endfunction

" NumberFold }}}2
" PatchFold {{{2

function! PatchFold()
        let h = getline(v:lnum)
        if match(h, '^diff') >= 0
                return ">1"
        elseif match(h, '^\@\@') >= 0
                return ">2" . len(h)
        else
                return "="
        endif
endfunction

" PatchFold }}}2
" RemoveTrailingWhitespaces {{{2

function! RemoveTrailingWhitespaces()
        silent! %s/\s\+$//e
endfunction

" RemoveTrailingWhitespaces }}}2
" Split declaration {{{2

" very basic splitting of c declaration from:
" [type] [ident] = [value];
" to:
" [type] [ident];
" [ident] = [value];
function! Split_Declaration()

        normal yypt=Bd^kt=dt;

endfunction

command! SplitDeclaration call Split_Declaration()
nnoremap gS :SplitDeclaration<CR>

" Split declaration }}}2

" functions }}}1
" commands {{{1

command! Wq wq
command! Wqa wqa
command! W w
command! Q q

command! Rtw call RemoveTrailingWhitespaces()

" commands }}}1
" autocmd {{{1

if has("autocmd")
        filetype plugin on

        aug vimrc
                au!

                au BufNewFile,BufReadPost *.md set ft=markdown

                " move quickfix window to bottom of screen
                au FileType qf wincmd J

                " resource vimrc on update
                au BufWritePost vimrc  source %

                " got this aug from derekwyatt's vimrc
                au BufReadPre   *.bin let &bin = 1
                au BufReadPost  *.bin if &bin | %!xxd
                au BufReadPost  *.bin set ft=xxd | endif
                au BufWritePre  *.bin if &bin | %!xxd -r
                au BufWritePre  *.bin endif
                au BufWritePost *.bin if &bin | %!xxd
                au BufWritePost *.bin set nomod | endif

                if version >= 702
                        au BufWinLeave * call clearmatches()
                endif

                if has('nvim')
                        au TermOpen * set nolist
                endif
        aug END
endif

" autocmd }}}1
" ~/.vimrc.local {{{1

if filereadable(expand("~/.vimrc.local"))
        source ~/.vimrc.local
endif

" ~/.vimrc.local }}}1
