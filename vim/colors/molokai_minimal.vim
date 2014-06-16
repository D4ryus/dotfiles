" Vim color file

hi clear

if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif

let g:colors_name="molokai_minimal"
"
" Support for 256-color terminal
"
if &t_Co > 15
   hi Normal          ctermfg=7   ctermbg=8
   hi CursorLine                  ctermbg=8   cterm=none
   hi CursorLineNr    ctermfg=10              cterm=none
   hi Boolean         ctermfg=6               cterm=bold
   hi Character       ctermfg=6               cterm=bold
   hi Number          ctermfg=6               cterm=bold
   hi String          ctermfg=6               cterm=bold
   hi Conditional     ctermfg=2               cterm=bold
   hi Constant        ctermfg=5               cterm=bold
   hi Cursor          ctermfg=16  ctermbg=7
   hi Debug           ctermfg=13              cterm=bold
   hi Define          ctermfg=14
   hi Delimiter       ctermfg=8

   hi DiffAdd                     ctermbg=12
   hi DiffChange      ctermfg=5   ctermbg=8
   hi DiffDelete      ctermfg=5   ctermbg=9
   hi DiffText                    ctermbg=8   cterm=bold

   hi Directory       ctermfg=10              cterm=bold
   hi Error           ctermfg=13  ctermbg=9
   hi ErrorMsg        ctermfg=13  ctermbg=16  cterm=bold
   hi Exception       ctermfg=10              cterm=bold
   hi Float           ctermfg=6               cterm=bold
   hi FoldColumn      ctermfg=14  ctermbg=16
   hi Folded          ctermfg=15  ctermbg=8
   hi Function        ctermfg=10
   hi Identifier      ctermfg=11              cterm=none
   hi Ignore          ctermfg=8   ctermbg=16
   hi IncSearch       ctermfg=7   ctermbg=16

   hi keyword         ctermfg=2               cterm=bold
   hi Label           ctermfg=7               cterm=none
   hi Macro           ctermfg=7
   hi SpecialKey      ctermfg=14

   hi MatchParen      ctermfg=16  ctermbg=3   cterm=bold
   hi ModeMsg         ctermfg=7
   hi MoreMsg         ctermfg=7
   hi Operator        ctermfg=2

   " complete menu
   hi Pmenu           ctermfg=6   ctermbg=16
   hi PmenuSel        ctermfg=7   ctermbg=8
   hi PmenuSbar                   ctermbg=16
   hi PmenuThumb      ctermfg=6

   hi PreCondit       ctermfg=10              cterm=bold
   hi PreProc         ctermfg=10
   hi Question        ctermfg=6
   hi Repeat          ctermfg=2               cterm=bold
   hi Search          ctermfg=15  ctermbg=8   cterm=NONE

   " marks column
   hi SignColumn      ctermfg=10  ctermbg=8
   hi SpecialChar     ctermfg=2               cterm=bold
   hi SpecialComment  ctermfg=8               cterm=bold
   hi Special         ctermfg=6
   if has("spell")
       hi SpellBad                ctermbg=1
       hi SpellCap                ctermbg=4
       hi SpellLocal              ctermbg=4
       hi SpellRare  ctermfg=none ctermbg=none  cterm=reverse
   endif
   hi Statement       ctermfg=2               cterm=bold
   hi StatusLine      ctermfg=8   ctermbg=7
   hi StatusLineNC    ctermfg=7   ctermbg=16
   hi StorageClass    ctermfg=2
   hi Structure       ctermfg=6
   hi Tag             ctermfg=2
   hi Title           ctermfg=11
   hi Todo            ctermfg=15  ctermbg=16  cterm=bold

   hi Typedef         ctermfg=6
   hi Type            ctermfg=11              cterm=none
   hi Underlined      ctermfg=7               cterm=underline

   hi VertSplit       ctermfg=7   ctermbg=16 cterm=bold
   hi VisualNOS                   ctermbg=8
   hi Visual                      ctermbg=8
   hi WarningMsg      ctermfg=15  ctermbg=8   cterm=bold
   hi WildMenu        ctermfg=6   ctermbg=16

   hi Comment         ctermfg=8
   hi CursorColumn                ctermbg=8
   hi ColorColumn                 ctermbg=8
   hi LineNr          ctermfg=7   ctermbg=8
   hi NonText         ctermfg=8

   hi SpecialKey      ctermfg=8
end

" Must be at the end, because of ctermbg=234 bug.
" https://groups.google.com/forum/#!msg/vim_dev/afPqwAFNdrU/nqh6tOM87QUJ
set background=dark
