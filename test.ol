type pairing = 
    | Pair of int * int 
    | Single of int 
    | Null;; 

let a = Pair (1, 2);;

let result : int = match a with 
    | Pair (x, y) => x + y
    | Single x => x 
    | Null => 0;;

let a : unit = print_string ("result " ^ string_of_int result);;