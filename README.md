# OCaml Lite

OCaml Lite is a programming langauge that is similar to OCaml but with some restrictions that makes it easier to parse and implement. This project implements a parser, interpreter, and typechecker for Ocaml Lite using Ocaml. *For CS 384 Programming Language and Implementation*

## How to run

Clone this repository. The `*.ol` files are example OCaml-Lite files that can be run. Alternatively, you could also write your own OCaml-Lite file.

```
cd ocaml-lite
dune exec ocaml-lite -- lc_parser.ol
```

## Implementation details

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

**Type Checker**

OCaml-lite uses a Hindley-Milner type system, so we implement a unification based type inference algorithm. This project uses the State monad to manage the typing constraints and context. The type inference rules are as follows: 

```
(x, t) in G         G |- e1 : t1 -> t2    G |- e2 : t1
----------- (Var)   ---------------------------------- (App)
G |- x : t                   G |- e1 e2 : t2

{(x, t1)} U G |- e : t2          G |- e1 : s    {(x, s)} U G |- e2 : t
----------------------- (Abs)    ------------------------------------- (Let)
G |- & x . e : t1 -> t2                G |- let x = e1 in e2 : t

G |- e : forall a. s           G |- e : s    a not free in G
-------------------- (Inst)    ----------------------------- (Gen)
   G |- e : s[t/a]                  G |- e : forall a. s
```

**Interpretor**

This project uses the State monad to manage the name-value bindings in the interpretor environment. The function evaluations are deferred until time of application, as shown below: 

```
----------------------------------
E |- fun x => e1 ==> (E, "fun x => e1")

E |- e1 ==> (E', "fun x => e3") E |- e2 ==> v1 E'[x -> v1] |- e3 ==> v2
---------------------------------------------------------------------------
E |- e1 e2 ==> v2
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

### Notes on interpretor

When adding name-value bindings to the interpretation environment (of structure (name*value) list), it is critical to append them to the beginning of the list. This results in desired behavior when we shadowing occurs. 

Specifically, if we introduce a variable in a certain scope (e.g. in match branch) that has the same name as a variable in the other scope (i.e. the name already has a binding in the environment), then we should expect the outer variable to be shadowed by the inner variable. This can be achieved by adding the new binding to the front of the list, since variable substitution always finds the first binding with the target name. 