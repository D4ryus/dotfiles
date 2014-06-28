" file: vimrc
" author: d4ryus - https://github.com/d4ryus/
" vim:ts=2:sw=2:foldmethod=marker:

" vundle {{{1

" settings {{{2

filetype off
set nocompatible
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" }}}2

Bundle 'gmarik/vundle'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-git'
Bundle 'godlygeek/tabular'
Bundle 'vim-scripts/taglist.vim'
Bundle 'sjl/gundo.vim'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'SirVer/ultisnips'
Bundle 'kien/ctrlp.vim'

" macros {{{1

map :fts zt,,f{azfa{j
map :haw :0r ~/.vim/license/haw.txt<CR>

" setter {{{1

syntax on                       " enable syntax highlighting
filetype plugin on              " enable filetype plugin
set ruler                       " show the cursor position all the time
set showcmd                     " display incomplete commands
set incsearch                   " do incremental searching
set ignorecase                  " dont use case sensetive search
set nocompatible                " set noncompatible mode (vi vim)
set number                      " set linenumber on left side
set relativenumber              " set a relative number scale on left side
set autoread                    " autoread file when changed from outside
set autoindent                  " always set autoindenting on
set list                        " list all tabs and ending spaces
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set wrap                        " do not insert line break
set expandtab                   " use spaces instead of tabs
set tabstop=4                   " amout of spaces per tab
set shiftwidth=4                " number of spaces used by autoindent
set cm=blowfish                 " use blowfish as encryption (X)
set history=82                  " keep 82 lines of command line history
set listchars=tab:>-,nbsp:_,trail:.
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set encoding=UTF-8              " use UTF-8 as encoding
set modelines=40                " search first/last 40 lines for vim modeline options
set laststatus=2                " allways show statusline
set spelllang=en,de             " set spelling language to english and german
set directory=~/.vim/swap       " directory where all swap files will be
set foldtext=NeatFoldText()
let g:EclimLoggingDisabled=1    " disable Eclim logging
let g:UltiSnipsEditSplit="vertical"
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

" commands {{{1

:command! WQ wq
:command! Wq wq
:command! W w
:command! Q q

" registers {{{1

let @f='"lyyO/* --fixme-- */"lpd2f|i/*A */==:w'
let @l='2fl"udwxxll"upa, jk'

" mapings {{{1

let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" noremap {{{2

noremap <Leader>p <Esc>:ProjectProblems<CR>
noremap <Leader>m :Ant magic<CR>
noremap <Leader>t :TlistToggle<CR>
noremap <Leader>u :GundoToggle<CR>

nnoremap <Up>    :res +1<CR>
nnoremap <Down>  :res -1<CR>
nnoremap <Left>  :vertical res -1<CR>
nnoremap <Right> :vertical res +1<CR>

" inoremap {{{2
"
inoremap jk <Esc>

" vnoremap {{{2

vnoremap < <gv
vnoremap > >gv

"}}}2

" abbrev {{{1

iabbrev date- <c-r>=strftime("%Y-%m-%d")<cr>
iabbrev file- <c-r>%<cr>

" color stuff {{{1

highlight ColorColumn ctermbg=grey
call matchadd('ColorColumn', '\%80v', 100)

if &term =~ "xterm" || &term =~ "screen" || &term =~ "urxvt" || &term =~ "rxvt-unicode-256color"
  colorscheme d4ryus_256
else
  colorscheme d4ryus_8
endif

" functions {{{1

function! NeatFoldText() "{{{2
" got this function from http://dhruvasagar.com/tag/vim thanks alot :)
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '| ' . printf("%10s", lines_count . ' lines') . ' |'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
" }}}2

function! MyFold() "{{{2
  let thisline = getline(v:lnum)
  if     match(thisline, '^\d\.\d\.\d\.\d\.\d') >= 0
    return ">5"
  elseif match(thisline, '^\d\.\d\.\d\.\d') >= 0
    return ">4"
  elseif match(thisline, '^\d\.\d\.\d') >= 0
    return ">3"
  elseif match(thisline, '^\d\.\d') >= 0
    return ">2"
  elseif match(thisline, '^\d') >= 0
    return ">1"
  else
    return "="
  endif
endfunction "}}}2

function! ApplyCodeStyle() "{{{2
  " fix else
  silent! %s/}[\r\n]else[\r\n]{/} else {/g
  " fix function
  silent! %s/.\+(.*).*\zs{\(.*$\)/\1\r{/g
  " remove all trailing whitespace's
  silent!  %s/\s\+$//e
  " retab
endfunction "}}}2

command! Codestyle call ApplyCodeStyle()

function! RemoveTrailingWhitespaces() "{{{2
  silent! %s/\s\+$//e
endfunction "}}}2

command! Rtw call RemoveTrailingWhitespaces()
