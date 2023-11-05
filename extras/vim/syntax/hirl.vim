" Quit when a syntax file was already loaded.
if exists('b:current_syntax') | finish |  endif

" Basic keywords
syn keyword hirl_keywords module
syn keyword hirl_keywords if
syn keyword hirl_keywords for
syn keyword hirl_keywords in
syn keyword hirl_keywords else
syn keyword hirl_keywords default
syn keyword hirl_keywords impl
syn keyword hirl_keywords enum
syn keyword hirl_keywords unused
syn keyword hirl_keywords tristate
syn keyword hirl_keywords register
syn keyword hirl_keywords super
syn keyword hirl_keywords root
syn keyword hirl_keywords package
syn keyword hirl_keywords use
syn keyword hirl_keywords cond
syn keyword hirl_keywords match
hi def link hirl_keywords Keyword

" Type-related keywords
syn keyword hirl_types wire
syn keyword hirl_types auto
syn keyword hirl_types bus
syn keyword hirl_types ubus
syn keyword hirl_types sbus
syn keyword hirl_types int
syn keyword hirl_types uint
syn keyword hirl_types bool
hi def link hirl_types Type

" Input/output
syn keyword hirl_directions input
syn keyword hirl_directions output
hi def link hirl_directions StorageClass

" Sensitivity
syn keyword hirl_sensitivity clock
syn keyword hirl_sensitivity sync
syn keyword hirl_sensitivity comb
syn keyword hirl_sensitivity async
syn keyword hirl_sensitivity const
syn keyword hirl_sensitivity signed
syn keyword hirl_sensitivity unsigned
hi def link hirl_sensitivity StorageClass

" Builtins 
syn keyword hirl_builtins min
syn keyword hirl_builtins max
syn keyword hirl_builtins trunc
syn keyword hirl_builtins ext
syn keyword hirl_builtins sext
syn keyword hirl_builtins zext
syn keyword hirl_builtins zeros
syn keyword hirl_builtins ones
syn keyword hirl_builtins join
syn keyword hirl_builtins rep
syn keyword hirl_builtins fold_or
syn keyword hirl_builtins fold_and
syn keyword hirl_builtins fold_xor
hi def link hirl_builtins Function

" Special register inputs
syn keyword hirl_reg_port clk
syn keyword hirl_reg_port next
syn keyword hirl_reg_port en
syn keyword hirl_reg_port nreset
syn keyword hirl_reg_port data
hi def link hirl_reg_port Underlined

" Operators
syn match hirl_operator '='
syn match hirl_operator /\~/
syn match hirl_operator /!/
syn match hirl_operator /\|/
syn match hirl_operator /&/
syn match hirl_operator /^/
syn match hirl_operator />/
syn match hirl_operator /</
syn match hirl_operator '?'
syn match hirl_operator /:/
syn match hirl_operator '+'
syn match hirl_operator /-/
syn match hirl_operator /\//
syn match hirl_operator /\*/
syn match hirl_operator /%/
syn match hirl_operator /<</
syn match hirl_operator />>/
syn match hirl_operator />=/
syn match hirl_operator /<=/
syn match hirl_operator /==/
syn match hirl_operator /!=/
syn match hirl_operator /&&/
syn match hirl_operator /||/
syn match hirl_operator /=>/
syn match hirl_operator '+:'
syn match hirl_operator /::/
syn match hirl_operator /:</
hi def link hirl_operator Operator

" Numeric constants
syn match hirl_nc /[0-9][a-zA-Z0-9_]*/
syn keyword hirl_bool true false
hi def link hirl_nc Number
hi def link hirl_bool Boolean

" Identifiers
syn match hirl_identifier /[a-zA-Z_][a-zA-Z0-9_]*/
hi def link hirl_identifier Identifier

" Constant IDs
syn match hirl_constant_id /[A-Z_][A-Z0-9_]*/
hi def link hirl_constant_id Constant

" TODOs in comments
syn match hirl_todo /TODO/ contained
hi def link hirl_todo Todo

" Comments
syn region hirl_block_comment start="/\*" end="\*/" fold transparent contains=hirl_todo
syn match hirl_comment "//.*$" contains=hirl_todo
syn match hirl_metadata_comment "//.*$" contains=hirl_todo
hi def link hirl_comment Comment
hi def link hirl_metadata_comment Comment
hi def link hirl_block_comment Comment

let b:current_syntax = 'hirl'
