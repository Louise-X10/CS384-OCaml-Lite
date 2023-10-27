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
%token <int> Id         (** Identifier, like a variable or function name *)
%token <int> Int        (** Integer literal *)
%token <string> String  (** String literal *)
%token EOF              (** End-of-file - you can ignore this *)

%start <program> start

%type <program> program

%%

start:
  | p = program; EOF; { p }
