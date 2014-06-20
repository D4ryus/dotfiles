" Vim color file

hi clear
if exists("syntax_on")
    syntax reset
endif

let g:colors_name="d4ryus"

" 0 = black
" 1 = red
" 2 = green
" 3 = yellow
" 4 = blue
" 5 = pink
" 6 = cyan
" 7 = white

hi Normal         ctermfg=7   ctermbg=0
hi CursorLine                 ctermbg=0
hi CursorLineNr   ctermfg=2
hi Boolean        ctermfg=6
hi Character      ctermfg=6
hi Number         ctermfg=6
hi String         ctermfg=6
hi Conditional    ctermfg=2
hi Constant       ctermfg=5
hi Cursor         ctermfg=0   ctermbg=7
hi Debug          ctermfg=5
hi Define         ctermfg=6
hi Delimiter      ctermfg=7

hi DiffAdd                    ctermbg=2
hi DiffChange     ctermfg=5   ctermbg=0
hi DiffDelete     ctermfg=0   ctermbg=1
hi DiffText                   ctermbg=0

hi Directory      ctermfg=2
hi Error          ctermfg=5   ctermbg=1
hi ErrorMsg       ctermfg=5   ctermbg=0
hi Exception      ctermfg=2
hi Float          ctermfg=6
hi FoldColumn     ctermfg=6   ctermbg=0
hi Folded         ctermfg=7   ctermbg=0
hi Function       ctermfg=2
hi Identifier     ctermfg=3
hi Ignore         ctermfg=7   ctermbg=0
hi IncSearch      ctermfg=7   ctermbg=0

hi keyword        ctermfg=2
hi Label          ctermfg=7
hi Macro          ctermfg=7
hi SpecialKey     ctermfg=6

hi MatchParen     ctermfg=0   ctermbg=7
hi ModeMsg        ctermfg=7
hi MoreMsg        ctermfg=7
hi Operator       ctermfg=2

" complete menu
hi Pmenu          ctermfg=6   ctermbg=0
hi PmenuSel       ctermfg=7   ctermbg=0
hi PmenuSbar                  ctermbg=0
hi PmenuThumb     ctermfg=6

hi PreCondit      ctermfg=2
hi PreProc        ctermfg=2
hi Question       ctermfg=6
hi Repeat         ctermfg=2
hi Search         ctermfg=7   ctermbg=0

" marks column
hi SignColumn     ctermfg=2   ctermbg=0
hi SpecialChar    ctermfg=2
hi SpecialComment ctermfg=7
hi Special        ctermfg=6

hi Statement      ctermfg=2
hi StatusLine     ctermfg=0   ctermbg=7
hi StatusLineNC   ctermfg=7   ctermbg=0
hi StorageClass   ctermfg=2
hi Structure      ctermfg=6
hi Tag            ctermfg=2
hi Title          ctermfg=3
hi Todo           ctermfg=7   ctermbg=0

hi Typedef        ctermfg=6
hi Type           ctermfg=3
hi Underlined     ctermfg=7

hi VertSplit      ctermfg=2   ctermbg=2
hi VisualNOS                  ctermbg=0
hi WarningMsg     ctermfg=7   ctermbg=0
hi WildMenu       ctermfg=6   ctermbg=0

hi Comment        ctermfg=7
hi CursorColumn               ctermbg=0
hi ColorColumn                ctermbg=0
hi LineNr         ctermfg=6   ctermbg=0
hi NonText        ctermfg=6   ctermbg=0

" trailing whitespaces
hi SpecialKey     ctermfg=5   ctermbg=0

if has("spell")
   hi SpellBad                 ctermbg=5
   hi SpellCap                 ctermbg=4
   hi SpellLocal               ctermbg=4
   hi SpellRare    ctermfg=none ctermbg=none cterm=reverse
endif
