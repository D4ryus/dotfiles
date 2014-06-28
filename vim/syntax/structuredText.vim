if exists("b:current_syntax")
    finish
endif

syntax keyword structuredTextKeyword IF THEN ELSE ELSIF END_IF
syntax keyword structuredTextKeyword PROGRAM END_PROGRAM
syntax keyword structuredTextKeyword WHILE END_WHILE
syntax keyword structuredTextKeyword FOR TO BY END_FOR
syntax keyword structuredTextKeyword VAR END_VAR VAR_CONFIG VAR_GLOBAL
syntax keyword structuredTextKeyword VAR_CONFIG END_VAR_CONFIG VAR_INPUT
syntax keyword structuredTextKeyword END_VAR_INPUT
syntax keyword structuredTextKeyword CASE OF END_CASE
syntax keyword structuredTextKeyword FUNCTION END_FUNCTION
syntax keyword structuredTextKeyword REPEAT END_REPEAT
syntax keyword structuredTextKeyword REPEAT END_REPEAT

syntax keyword structuredTextBoolean TRUE FALSE

syntax keyword structuredTextFunction PRINT SIN COS TAN ASIN ACOS ATAN
syntax keyword structuredTextFunction EXP LOG LN SQRT ABS ADD MUL SUB DIV
syntax keyword structuredTextFunction MOD EXPT AND OR XOR NOT

syntax keyword structuredTextIdentifier BOOL INT SINT TON WORD TIME REAL BYTE
syntax keyword structuredTextIdentifier LREAL DINT LINT USINT UINT UDINT ULINT
syntax keyword structuredTextIdentifier LREAL DATE STRING LWORD DWORD WSTRING

syntax match structuredTextNumber "\m[ =#]\zs\d\+\ze[^a-zA-Z]"

syntax match structuredTextComment "\m(\*.*\*)"

syntax match structuredTextString "\m\".\{-}\""

highlight link structuredTextString     String
highlight link structuredTextFunction   Function
highlight link structuredTextBoolean    Boolean
highlight link structuredTextIdentifier Identifier
highlight link structuredTextNumber     Number
highlight link structuredTextKeyword    Keyword
highlight link structuredTextComment    Comment

let b:current_syntax = "structuredText"
