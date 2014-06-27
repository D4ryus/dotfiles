if exists("b:current_syntax")
    finish
endif

syntax keyword structuredTextKeyword IF THEN ELSE ELSIF END_IF
syntax keyword structuredTextKeyword PROGRAM END_PROGRAM
syntax keyword structuredTextKeyword WHILE END_WHILE
syntax keyword structuredTextKeyword FOR TO BY END_FOR
syntax keyword structuredTextKeyword VAR END_VAR VAR_CONFIG VAR_GLOBAL VAR_CONFIG
syntax keyword structuredTextKeyword CASE OF END_CASE
syntax keyword structuredTextKeyword FUNCTION END_FUNCTION
syntax keyword structuredTextKeyword REPEAT END_REPEAT
syntax keyword structuredTextKeyword REPEAT END_REPEAT

syntax keyword structuredTextBoolean TRUE FALSE

syntax keyword structuredTextFunction SIN COS TAN ASIN ACOS ATAN EXP LOG LN SQRT ABS ADD MUL SUB DIV MOD EXPT AND OR XOR NOT

syntax keyword structuredTextIdentifier BOOL INT SINT TON WORD TIME REAL BYTE LREAL DINT LINT USINT UINT UDINT ULINT LREAL DATE STRING LWORD DWORD WSTRING

syntax keyword structuredTextMacro PRINT

syntax match structuredTextSpecial "PROGRAM.*"

syntax match structuredTextNumber "\v\W\zs\d+"

syntax match structuredTextComment "\v\(\*.*\*\)"

syntax match structuredTextString "\v\".{-}\""

highlight   link   structuredTextString       String
highlight   link   structuredTextFunction     Function
highlight   link   structuredTextBoolean      Boolean
highlight   link   structuredTextMacro        Macro
highlight   link   structuredTextSpecial      Special
highlight   link   structuredTextIdentifier   Identifier
highlight   link   structuredTextNumber       Number
highlight   link   structuredTextKeyword      Keyword
highlight   link   structuredTextComment      Comment

let b:current_syntax = "sturcturedText"
