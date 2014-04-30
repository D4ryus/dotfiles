" file: vimrc
" author: d4ryus - https://github.com/d4ryus/
" vim:ts=2:sw=2:

" vundle ----------------------------------------------------------

set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'msanders/snipmate.vim'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'Lokaltog/vim-powerline'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'dhruvasagar/vim-table-mode'
Bundle 'tpope/vim-surround'
Bundle 'tomasr/molokai'

filetype plugin on


" abbreviations ----------------------------------------------------

iabbr author author: d4ryus - https://github.com/d4ryus/
iabbr file: file: <c-r>%
iabbr #i #include
iabbr #d #define


" registers --------------------------------------------------------

" recursive fold macro
map :fts    zt,,f{azfa{j
" insert license
map :chaw   :0r ~/.vim/license/haw.txt
" i dont wanna fix this shit - macro
map :idwtfts "lyy<CR>O/* --warning-- */<ESC>"lpi/* <ESC>d2f\|A */<ESC>==:<CR>:w<CR>

" setter -----------------------------------------------------------

syntax on                       " enable syntax highlighting
set cm=blowfish                 " use blowfish as encryption (X)
set autoindent                  " always set autoindenting on
set history=82                  " keep 82 lines of command line history
set ruler                       " show the cursor position all the time
set showcmd                     " display incomplete commands
set incsearch                   " do incremental searching
set hlsearch                    " highlight search
set ignorecase                  " dont use case sensetive search
set nocompatible                " set noncompatible mode (vi vim)
set relativenumber              " set a relative number scale on left side
set expandtab                   " use spaces instead of tabs
set autoread                    " autoread file when changed from outside
set tabstop=4                   " amout of spaces per tab
set shiftwidth=4                " number of spaces used by autoindent
set listchars=tab:>~,nbsp:_,trail:.
set list                        " list all tabs and ending spaces
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set encoding=UTF-8              " use UTF-8 as encoding
set laststatus=2                " always show statusbar, since its powerline
set t_Co=256                    " set Terminal color to 256
set nowrap                      " do not insert line break


" mapings ----------------------------------------------------------

let mapleader=','

map <Tab> %
map <Leader>h <Esc>:tabprevious<CR>
map <Leader>l <Esc>:tabnext<CR>
map <Leader>n <Esc>:NERDTreeToggle<CR>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>
nnoremap <Leader>m :noh<CR>
nnoremap <silent> <F8> :NERDTreeToggle<CR>

imap jk <Esc>

inoremap <C-U> <C-G>u<C-U>

vnoremap < <gv
vnoremap > >gv


" random ----------------------------------------------------------

" make 81st column stand out
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

" save files when focus is lost
au FocusLost * :wa<CR>

if has('mouse')
  set mouse=a
endif

set background=dark             " set Terminal background dark, so that molokai looks pretty
colorscheme molokai

" open NERDTree if no file is specified
autocmd vimenter * if !argc() | NERDTree | endif
