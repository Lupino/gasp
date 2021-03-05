" Vim syntax file
" Language: gasp
" Maintainer: Li Meng Jun <lmjubuntu@gamil.com>
" Latest Revision: 2021-03-05

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

" Values
syn match gaspIdentifier "\<\(attr\|metric\|gpio\)_\(\w\|\'\)*\>"

syn match gaspNumber "0[xX][0-9a-fA-F]\+\|0[oO][0-7]\|[0-9]\+"
syn match gaspFloat "[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\="
syn keyword gaspBoolean true false
syn region gaspString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell

" Delimiters
syn match gaspDelimiter "[,;|.()[\]{}]"

syn match gaspOperator "\([-!#$%&\*\+/<=>\?@\\^|~:]\|\<_\>\)"
syn match gaspComment "//*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=@Spell

syn keyword gaspApp     app
syn keyword gaspInit    init
syn keyword gaspSetup   setup
syn keyword gaspLoop    loop
syn keyword gaspGpio    gpio
syn keyword gaspFunc    func
syn keyword gaspEvery   every
syn keyword gaspCommand command
syn keyword gaspAttr    attr
syn keyword gaspMetric  metric
syn keyword gaspRule    rule
syn keyword gaspLink    link
syn keyword gaspClick   click

syn keyword gaspConditional rule do later else on
syn keyword gaspStatement do done

syn sync fromstart

if version >= 508 || !exists("did_hs_syntax_inits")
  if version < 508
    let did_hs_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink gaspIdentifier Identifier

  HiLink gaspBoolean Boolean
  HiLink gaspNumber Number
  HiLink gaspFloat Float
  HiLink gaspString String

  HiLink gaspDelimiter Delimiter

  HiLink gaspApp     Keyword
  HiLink gaspInit    Keyword
  HiLink gaspSetup   Keyword
  HiLink gaspLoop    Keyword
  HiLink gaspGpio    Keyword
  HiLink gaspFunc    Keyword
  HiLink gaspEvery   Keyword
  HiLink gaspCommand Keyword
  HiLink gaspAttr    Keyword
  HiLink gaspMetric  Keyword
  HiLink gaspRule    Keyword
  HiLink gaspLink    Keyword
  HiLink gaspClick   Keyword
  HiLink gaspConditional Conditional
  HiLink gaspStatement Statement
  HiLink gaspComment Comment
  HiLink gaspOperator Operator

  delcommand HiLink
endif

let b:current_syntax = "gasp"
