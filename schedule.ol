type day = | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday;;

let weekday (d: day) : bool = 
  match d with
  | Saturday => false 
  | Sunday => false
  | Monday => true 
  | Tuesday => true 
  | Wednesday => true 
  | Thursday => true 
  | Friday => true ;;

let toggle_representation : day -> int =
  fun d =>
    match d with
    | Monday => 1
    | Tuesday => 2
    | Wednesday => 3
    | Thursday => 4
    | Friday => 5
    | Saturday => 6 
    | Sunday => 7 ;;

(* name * day_of_week * time *)
type event = | Event of string * day * int ;;

type event_list =
| Nil
| Cons of event * event_list ;;

let rec print_schedule (lst: event_list) : unit = 
  match lst with 
  | Cons (e, r) => 
    let _ = (match e with 
    | Event(name, day, time) => 
      print_string ("Day " ^ (string_of_int (toggle_representation day)) ^ " at " ^ string_of_int time ^ " o'clock : " ^ name)) in 
    print_schedule r
  | Nil => ();;

let my_list = 
  Cons(Event("School", Monday, 8),
    Cons(Event("School", Friday, 8), 
      Cons(Event("School", Tuesday, 8), 
        Cons(Event("School", Wednesday, 8), 
          Cons(Event("Tutor", Tuesday, 19), Nil)
        )
      )
    )
  );;
  
let a = print_schedule my_list;;