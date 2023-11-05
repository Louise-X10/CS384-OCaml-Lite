%{
    open Ast
%}

%token Type             (** type - keyword *)
%token Of               (** of - keyword *)
%token Let              (** let - keyword *)
%token Rec              (** rec - keyword *)
%token In               (** in - keyword *)
%token If               (** if - keyword *)
%token Then             (** then - keyword *)
%token Else             (** else - keyword *)
%token Match            (** match - keyword *)
%token With             (** with - keyword *)
%token Fun              (** fun - keyword *)
%token True             (** true - keyword *)
%token False            (** false - keyword *)
%token Mod              (** mod - keyword *)
%token TInt             (** int - type name *)
%token TBool            (** bool - type name *)
%token TString          (** string - type name *)
%token TUnit            (** unit - type name *)
%token Eq               (** = - binary operator *)
%token Plus             (** + - binary operator *)
%token Minus            (** - - binary operator *)
%token Times            (** * - binary operator *)
%token Divide           (** / - binary operator *)
%token Lt               (** < - binary operator *)
%token Concat           (** ^ - binary operator *)
%token And              (** && - binary operator *)
%token Or               (** |%token - binary operator *)
%token Not              (** ! - unary operator *)
%token Negate           (** ~ - unary operator *)
%token DoubleSemicolon  (** ;; *)
%token Colon            (** : *)
%token Arrow            (** -> *)
%token DoubleArrow      (** => *)
%token LParen           (** ( *)
%token RParen           (** ) *)
%token Pipe             (** %token *)
%token Comma            (** , *)
%token <string> Id      (** Identifier, like a variable or function name *)
%token <int> Int        (** Integer literal *)
%token <string> String  (** String literal *)
%token EOF              (** End-of-file - you can ignore this *)

%nonassoc Arrow /* Function type -> has lower precedence than tuple type * */
%nonassoc DoubleArrow In Else /* Match, Let, Else statement all have lower precendence than +-* */
%left Or
%left And
%nonassoc Eq Lt
%left Plus Minus Concat
%left Times Divide Mod
%nonassoc Negate Not

%start <program> start
%start <expr> start_expr

%type <program> program
%type <binding> binding
%type <typ_binding> typ_binding
%type <param> param
%type <expr> expr application base
%type <expr list> expr_tuple
%type <typ> typ typ_base 
%type <typ list> typ_tuple
%type <matchbranch list> match_branches
%type <pattern_vars> pattern_vars

%%

start:
  | p = program; EOF; { List.rev p } 

start_expr:
  | e = expr; EOF; { e } 

/* 
<program> ::= [<binding> ;;]+

<binding> ::= let $id [<param>]* [: <type>] = <expr>
            | let rec $id [<param>]* [: <type>] = <expr>
            | type $id = <typ_bindings>

<typ_bindings> ::= ['|' $id [of <type>]]+

<param> ::= $id
          | ( $id : <type> )
 */

program: 
  | b = binding;                  { [b] }
  | bs = program; b = binding;    { b :: bs }

binding:
  | Let; r = boption(Rec); x = Id; ps = param*; Colon; t = typ;Eq; e = expr; DoubleSemicolon;   { LetB(x, r, ps, Some t, e)}
  | Let; r = boption(Rec); x = Id; ps = param*; Eq; e = expr; DoubleSemicolon;                  { LetB(x, r, ps, None, e )}
  | Type; x = Id; Eq; ts = typ_binding+; DoubleSemicolon;                         { TypeB(x, List.rev ts)}

typ_binding:
  | Pipe; x = Id               { (x, None)}
  | Pipe; x = Id; Of; t = typ  { (x, Some t)}

param:
  | LParen; x = Id; Colon; t = typ; RParen; { { name = x; p_type = Some t } }
  | x = Id                                  { { name = x; p_type = None } }

/* 
<expr> ::= let $id [<param>]* [: <type>] = <expr> in <expr>
         | let rec $id [<param>]* [: <type>] = <expr> in <expr>
         | if <expr> then <expr> else <expr>
         | fun [<param>]+ [: <type>] => <expr>
         | <expr> <binop> <expr>
         | <unop> <expr>
         | ( <expr> [, <expr>]+ )
         | match <expr> with <match_branches>
         # | <expr> <expr> # change to <app>
         | <app>

<app> ::= <app> <base> | <base> # right-associative

<base> ::=          
         | ( <expr> )
         | $int
         | true
         | false
         | $string
         | $id
         | ( )

<binop> ::= + | - | * | / | mod | < | = | ^ | && | ||

<unop> ::= not | ~
 */

expr:
  | Let; r = boption(Rec); x = Id; ps = param*; Colon; t = typ; Eq; e1 = expr; In; e2 = expr; { LetExp(x, r, List.rev ps, Some t, e1, e2) }
  | Let; r = boption(Rec); x = Id; ps = param*; Eq; e1 = expr; In; e2 = expr;              { LetExp(x, r, List.rev ps, None, e1, e2) }
  | If; e1 = expr; Then; e2 = expr; Else; e3 = expr;                          { IfExp(e1,e2,e3) }
  | Fun; ps = param+; Colon; t = typ; DoubleArrow; e = expr;          { Function(List.rev ps, Some t, e) }
  | Fun; ps = param+; DoubleArrow; e = expr;                          { Function(List.rev ps, None, e) }
  | Match; e = expr; With; bs = match_branches;                               { MatchExp(e, bs) } 
  | e1 = expr; Plus; e2 = expr;                                               { Add(e1,e2) }
  | e1 = expr; Minus; e2 = expr;                                              { Sub(e1,e2) }
  | e1 = expr; Times; e2 = expr;                                              { Mul(e1,e2) }
  | e1 = expr; Divide; e2 = expr;                                             { Div(e1,e2) }
  | e1 = expr; Mod; e2 = expr;                                                { Modulo(e1,e2) }
  | e1 = expr; Lt; e2 = expr;                                                 { LessThan(e1,e2) }
  | e1 = expr; Eq; e2 = expr;                                                 { Equal(e1,e2) }
  | e1 = expr; Concat; e2 = expr;                                             { StrConcat(e1,e2) }
  | e1 = expr; And; e2 = expr;                                                { LogicAnd(e1,e2) }
  | e1 = expr; Or; e2 = expr;                                                 { LogicOr(e1,e2) }
  | Not; e = expr;                                                            { LogicNegate(e) }
  | Negate; e = expr;                                                         { IntNegate(e) } 
  | LParen; es = expr_tuple; RParen;                                          { Tuple(List.rev es) }
  | a = application;                                                          { a }

expr_tuple: 
  | e1 = expr; Comma; e2 = expr;               { [e2; e1] }
  | es = expr_tuple; Comma; e = expr;          { e :: es }

application:
  | a = application; b = base; { App(a, b) }
  | b = base;                  { b }

base:
  | LParen; e = expr; RParen;                                                 { e }
  | n = Int;                                                                  { CInt n }
  | True;                                                                     { CBool true }
  | False;                                                                    { CBool false }
  | s = String;                                                               { CString s }
  | x = Id;                                                                   { Var x }
  | LParen; RParen;                                                           { Unit }

/* 
<match_expr> ::=  match <expr> with <match_branches>
<match_branches> ::= ['|' <match_branch>]+
*/

match_branches: 
| Pipe; x = Id; vs = option(pattern_vars); DoubleArrow; e = expr;                        { [MatchBr(x, vs, e)] }
| Pipe; x = Id; vs = option(pattern_vars); DoubleArrow; e = expr; bs = match_branches;   { MatchBr(x, vs, e):: bs }

/* 
<pattern_vars_opt> ::= $id | ( $id [, $id ]+ )
 */
pattern_vars:
  | x = Id;                                                 { [x] }
  | LParen; xs =separated_nonempty_list(Comma, Id); RParen; { xs }

/* 
<typ> ::=
  | <type> -> <type>
  | <typ_tuple>
  | <typ_base>

<typ_tuple> ::= 
  | <typ_tuple> * <typ_base>
  | <typ_base> * <typ_base>  

<typ_base> ::= 
         | ( <type> )
         | int
         | bool
         | string
         | unit
         | $id
*/

typ: 
  | t1 = typ; Arrow; t2 = typ;  { FuncTy (t1, t2) }
  | ts = typ_tuple;             { TupleTy (List.rev ts)}
  | t = typ_base;               { t }

typ_base:
  | TInt;                       { IntTy }
  | TBool;                      { BoolTy }
  | TString;                    { StringTy }
  | TUnit;                      { UnitTy }
  | x = Id;                     { CustomTy x }
  | LParen; t = typ; RParen;    { t }

typ_tuple:
  | t1 = typ_base; Times; t2 = typ_base   { [t2; t1]}
  | ts = typ_tuple; Times; t = typ_base   { t :: ts}
