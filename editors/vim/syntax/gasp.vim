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
syn match gaspIdentifier "\<\(attr\|metric\|gpio\)_[A-Za-z0-9_]*\>"
syn match gaspType "\<\(boolean\|bool\|char\|unsigned\|byte\|int\|word\|long\|short\|float\|double\|uint8_t\|uint16_t\|uint32_t\|const\)\>"

syn match gaspNumber "0[xX][0-9a-fA-F]\+\|0[oO][0-7]\|[0-9]\+"
syn match gaspFloat "[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\="
syn keyword gaspBoolean true false
syn region gaspString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell

" Delimiters
syn match gaspDelimiter "[,;|.()[\]{}]"

syn match gaspOperator "\([-!#$%&\*\+/<=>\?@\\^|~:]\|\<_\>\)"
syn match gaspComment "//*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=@Spell

syn keyword gaspLink    link
syn keyword gaspClick   click
syn keyword gaspLater   later

syn match gaspSection "^\(app\|setup\|loop\|agpio\|gpio\|func\|every\|command\|attr\|attr\|metric\|rule\|import\|flag\|data\|require\|uart\|raw\|fd\)"

syn keyword gaspConditional do else on

syn sync fromstart

if version >= 508 || !exists("did_hs_syntax_inits")
  if version < 508
    let did_hs_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink gaspIdentifier Identifier

  HiLink gaspType Type

  HiLink gaspBoolean Boolean
  HiLink gaspNumber Number
  HiLink gaspFloat Float
  HiLink gaspString String

  HiLink gaspDelimiter Delimiter

  HiLink gaspLink    Keyword
  HiLink gaspClick   Keyword
  HiLink gaspLater   Keyword
  HiLink gaspConditional Conditional
  HiLink gaspComment Comment
  HiLink gaspOperator Operator

  HiLink gaspSection Structure

  delcommand HiLink
endif

let b:current_syntax = "gasp"
