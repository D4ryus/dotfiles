" file: ~/.vimrc
" author: d4ryus - https://github.com/d4ryus/
" vim:ts=8:sw=8:foldmethod=marker:

" vundle {{{1

" settings {{{2

filetype off
set nocompatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" settings }}}2

Bundle 'gmarik/vundle'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-sleuth'
Bundle 'tpope/vim-vinegar'
Bundle 'dhruvasagar/vim-table-mode'
Bundle 'gregsexton/gitv'
Bundle 'godlygeek/tabular'
Bundle 'kien/ctrlp.vim'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'AlxHnr/clear_colors'
Bundle 'andrwb/vim-lapis256'
Bundle 'majutsushi/tagbar'

if executable('ctags')
        Bundle 'ludovicchabant/vim-gutentags'
        Bundle 'vim-scripts/taglist.vim'
endif

if has("python")
        Bundle 'kovisoft/slimv'
endif

if has("python") || has("python3")
        Bundle 'SirVer/ultisnips'
        Bundle 'sjl/gundo.vim'
endif

" vundle }}}1
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
" Taglist {{{2

let Tlist_Use_Right_Window = 1
let Tlist_File_Fold_Auto_Close = 1

noremap cot :TagbarToggle<CR>

" Taglist }}}2
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

let g:slimv_repl_split = 2

noremap cop :RainbowParenthesesToggle<CR>

" Slimv }}}2

" plugin-settings }}}1
" setter {{{1

syntax on                       " enable syntax highlighting
set showcmd                     " display incomplete commands
set incsearch                   " do incremental searching
set ignorecase                  " dont use case sensetive search
set nocompatible                " set noncompatible mode (vi vim)
set autoread                    " autoread file when changed from outside
set smartindent                 " should work better than autoindent
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set list                        " list all tabs and ending spaces
set listchars=tab:>·,nbsp:_,trail:·,precedes:<,extends:>,eol:¬,conceal:_
set linebreak                   " better wraping of lines
set showbreak=➣➣                " show linebreaks if wrap is set
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
set completeopt=longest,menuone " dont select the first match
set splitright                  " open splits on the right side instead of left
set splitbelow                  " open splits blow instead of on top
set wildmenu                    " use wildmenu
set wildmode=longest:list,full  " dont change insertion
set path+=/usr/local/include    " also search through /usr/local/include
set path+=./**                  " also add the current path
set diffopt=vertical            " vertical diff
set nojoinspaces                " do not add spaces on join
set formatoptions-=o            " do not continue comment after hitting 'o'
set wildignore+=*.o,*.obj,*.class " ignore binary files
set ttyfast                     " assume a fast terminal
set display=lastline            " show as much wrapped lines as possible

" move cursor everywhere in visual block mode
if has('virtualedit')
  set virtualedit+=block
endif

if has('unnamedplus')
        set clipboard=unnamed,unnamedplus
else
        set clipboard=unnamed
endif

" use blowfish as encryption (:X)
if !has('nvim')
        if v:version >= 704
                set cm=blowfish2
        elseif v:version >= 702
                set cm=blowfish
        endif
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

nnoremap <Up>    :res +1<CR>
nnoremap <Down>  :res -1<CR>
nnoremap <Left>  :vertical res -1<CR>
nnoremap <Right> :vertical res +1<CR>

nnoremap gb :ls<CR>:b<Space>

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

set background=light

" background }}}2
" statusline {{{2

set statusline=%<[%F]
set statusline+=\ [%{FileSize()}]
set statusline+=\ [%{&ff}]
set statusline+=\ [%M%Y%R%q%W]
set statusline+=\ %{fugitive#statusline()}%=
set statusline+=\ [pos:\ %l/%L:%v\ %p%%]

" statusline }}}2
" matchparen {{{2

highlight MatchParen ctermbg=0 ctermfg=1 cterm=bold

" matchparen }}}2

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
" Bash {{{2

function! Bash()
        let prompt = '>'
        call search(prompt, 'b')
        let cmd = substitute(getline('.'), '^.*' . prompt, '', '')
        set paste
        call append(line('.'), ['', '', prompt])
        call append(line('.') + 1, systemlist(cmd))
        set nopaste
        call search('^' . prompt)
endfunction

" Bash }}}2
" Overlength {{{2

function! Overlength_toggle()
        if exists('g:overlength_enabled')
                let g:overlength_enabled = 0
                highlight OverLength none
        else
                let g:overlength_enabled = 1
                highlight OverLength ctermbg=6 guibg=#592929
                match OverLength /\%81v.\+/
        endif
endfunction

nnoremap coo :OverlengthToggle<CR>

" Overlength }}}2

" functions }}}1
" commands {{{1

command! WQ wq
command! Wq wq
command! W w
command! Q q

command! Rtw call RemoveTrailingWhitespaces()
command! Bash call Bash()
command! OverlengthToggle call Overlength_toggle()

" commands }}}1
" autocmd {{{1

if has("autocmd")
        filetype plugin on

        aug clearMatches
                if version >= 702
                        au BufWinLeave * call clearmatches()
                endif
        aug END

        aug filetypes
                au!
                au BufNewFile,BufReadPost *.md set filetype=markdown
                au FileType qf wincmd J
        aug END

        aug vimrc
                au!
                au BufWritePost vimrc  source %
        aug END

        " got this aug from derekwyatt's vimrc
        aug Binary
                au!
                au BufReadPre   *.bin let &bin = 1
                au BufReadPost  *.bin if &bin | %!xxd
                au BufReadPost  *.bin set filetype=xxd | endif
                au BufWritePre  *.bin if &bin | %!xxd -r
                au BufWritePre  *.bin endif
                au BufWritePost *.bin if &bin | %!xxd
                au BufWritePost *.bin set nomod | endif
        aug END

if has('nvim')
        aug Nvim
                au TermOpen * set nolist
        aug END
endif
endif

" autocmd }}}1
" ~/.vimrc.local {{{1

if filereadable(expand("~/.vimrc.local"))
        source ~/.vimrc.local
endif

" ~/.vimrc.local }}}1
