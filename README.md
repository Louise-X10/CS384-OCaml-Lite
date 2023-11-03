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

### Notes on parser generator

The parser generator has a shift/reduce conflict due to ambiguous grammar of parsing match branches. Luckily, the parser arbitrary resolves the conflict to yield the desired behavior. 

```
(* Current grammar *)
<match_branches> ::= '|' id => <expr> 
                  |  '|' id => <expr>  <match_branches>

(* Goal *)
Disambiguate grammar so match is right associative: 
  match .. | x=>  match .. | y => e2 | z => e3 should be parsed as
  match .. | x=> (match .. | y => e2 | z => e3), should not be parsed as 
  match .. | x=> (match .. | y => e2) | z => e3

i.e. If encounter another 'match' keyword, remaining pipes should belong to inner match expr. 
```

The AST type of `TupleTy` is a list of types so that there can be an arbitrary amount of types in a tuple, similar to how the AST type of a `Tuple` is a list of expressions. The type grammar is further disambiguated to distinguish between `int * int * int`, `(int * int) * int`, and `int * (int * int)`.  

```
I.e. Disambiguate so <typ_pair> cannot call <typ_tuple>. 
I.e. <typ_tuple> can only resolve to unique list of basic types

<typ> ::=
  | <type> -> <type>
  | <typ_tuple>
  | <typ_base>

<typ_tuple> ::= 
  | <typ_tuple> * <typ_base>
  | <typ_pair>

<typ_pair> :: <typ_base> * <typ_base>

<typ_base> ::= 
         | ( <type> )
         | int
         | bool
         | string
         | unit
         | $id
```