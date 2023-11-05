open Ast

exception TypeError of string

let typecheck (e: program) : typ = 
    failwith "undefined"

let typecheck_expr (e: expr) : typ = 
  failwith "undefined"
