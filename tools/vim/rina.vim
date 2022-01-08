" Vim syntax file
" Language: Catrina
" Maintainer: Maxime emixampons@gmail.com
" Latest Revision: 4 January 2021

" FIXME(Maxime): function name ids

if exists("b:current_syntax")
  finish
endif

syntax match rinaComment "$--.*"
" FIXME(Maxime): multiline comments

syntax keyword rinaKeyword ar ob

" FIXME(Maxime)
syntax match rinaKeyword "{=}" 
syntax match rinaKeyword "{:}"
syntax match rinaKeyword "{ }"
syntax region rinaString start=/\v"/ skip=/\v\\./ end=/\v"/
syntax match rinaNumber  "\v[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)"
syntax match rinaBigName "[_A-Z][_a-zA-Z0-9]*"
syntax match rinaIdent   "[_a-z][_a-zA-Z0-9]*"
syntax match rinaBuiltin "\v\@[_a-z][_a-zA-Z0-9]*"
syntax match rinaBuiltin "\.[_a-z][_a-zA-Z0-9]*"
syntax match rinaBuiltin "[_a-z][_a-zA-Z0-9]*\."

syntax match rinaOperator "\v\+"
syntax match rinaOperator "\v\-"
syntax match rinaOperator "\v\*"
syntax match rinaOperator "\v/"
syntax match rinaOperator "\v\="
syntax match rinaOperator "\v:" 
syntax match rinaOperator "\v-\>" 
syntax match rinaOperator "\v'"
syntax match rinaOperator "\v\$"

syntax match rinaLabel  '[_a-z][_a-zA-Z0-9]*\( *=\)\@='
syntax match rinaLabel  '[_a-z][_a-zA-Z0-9]*\( *:\)\@='

highlight link rinaComment    Comment
highlight link rinaBigName    Special
highlight link rinaIdent      Other
highlight link rinaLabel      PreCondit
highlight link rinaNumber     Number
highlight link rinaString     Number
highlight link rinaKeyword    Statement
highlight link rinaOperator   Operator
highlight link rinaBuiltin    Function

let b:current_syntax = "catrina"
