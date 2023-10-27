# OCaml Lite

OCaml Lite is a programming langauge that is similar to OCaml but with some restrictions that makes it easier to parse and implement. This project implements a parser, interpreter, and typechecker for Ocaml Lite using Ocaml. *For CS 384 Programming Language and Implementation*

**Syntax**

The grammar for Ocaml Lite:

```
<program> ::= [<binding> ;;]+

<binding> ::= let $id [<param>]* [: <type>] = <expr>
            | let rec $id [<param>]* [: <type>] = <expr>
            | type $id = ['|' $id [of <type>]]+

<param> ::= $id
          | ( $id : <type> )

<expr> ::= let $id [<param>]* [: <type>] = <expr> in <expr>
         | let rec $id [<param>]* [: <type>] = <expr> in <expr>
         | if <expr> then <expr> else <expr>
         | fun [<param>]+ [: <type>] => <expr>
         | <expr> <expr>
         | ( <expr> [, <expr>]+ )
         | <expr> <binop> <expr>
         | <unop> <expr>
         | ( <expr> )
         | $int
         | true
         | false
         | $string
         | $id
         | ( )
         | match <expr> with ['|' <match_branch>]+

<binop> ::= + | - | * | / | mod | < | = | ^ | && | ||

<unop> ::= not | ~

<type> ::= <type> -> <type>
         | ( <type> )
         | <type> * <type>
         | int
         | bool
         | string
         | unit
         | $id

<match_branch> ::= $id [<pattern_vars>] => <expr>

<pattern_vars> ::= $id
                 | ( $id [, $id ]+ )
```

The precedence of operators are as follows:
```
(Highest precedence)
not
~
*, /, mod
+, -, ^
<, =
&&
||
(Lowest precedence)
```