" vundle ----------------------------------------------------------

set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
" alternatively, pass a path where Vundle should install bundles
" let path = '~/some/path/here'
" call vundle#rc(path)

Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'msanders/snipmate.vim'
Bundle 'mattn/webapi-vim'
Bundle 'mattn/gist-vim'
Bundle 'vim-scripts/taglist.vim'
Bundle 'Lokaltog/vim-powerline'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'dhruvasagar/vim-table-mode'
Bundle 'Raimondi/delimitMate'
Bundle 'tpope/vim-surround'
Bundle 'tomasr/molokai'

filetype plugin on

" vundle settings --------------------------------------------------

"let g:Powerline_symbols = 'fancy'

" abbreviations ----------------------------------------------------

iabbr -email- w.wackerbauer@yahoo.de
iabbr -name- Wackerbauer Wolfgang
iabbr #i #include
iabbr #d #define

" switches ---------------------------------------------------------

syntax on

" setter -----------------------------------------------------------

set cm=blowfish
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
set shiftwidth=4
set listchars=tab:>~,nbsp:_,trail:.
set list                        " list all tabs and ending spaces
set nobackup                    " do not create backups
set nowritebackup               " also no write backups
set backspace=indent,eol,start  "allow backspacing over everything in insert mode
set encoding=UTF-8              " use UTF-8 as encoding
set laststatus=2                " always show statusbar, since its powerline
set t_Co=256                    " set Terminal color to 256
set background=dark             " set Terminal background dark, so that molokai looks pretty
set nowrap                      " do not insert line break


" mapings ----------------------------------------------------------
"
let mapleader=','

map :haw :0r ~/.vim/license/haw.txt
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

vmap  <expr>  <Left>   DVB_Drag('left') 
vmap  <expr>  <Right>  DVB_Drag('right')
vmap  <expr>  <Down>   DVB_Drag('down') 
vmap  <expr>  <Up>     DVB_Drag('up')   
vmap  <expr>  .        DVB_Duplicate()  

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

colorscheme molokai
