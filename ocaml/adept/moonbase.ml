type ('lock, 'suit, 'location, 'pockets) state = State

let begin_day () = print_endline "DAY BEGINS"; State
let open_lock _ = print_endline "OPENING LOCK"; State
let close_lock _ = print_endline "CLOSING LOCK"; State
let spacesuit_on _ = print_endline "SPACESUIT ON"; State
let spacesuit_off _ = print_endline "SPACESUIT OFF"; State
let leave_base _ = print_endline "LEAVING BASE"; State
let enter_base _ = print_endline "ENTERING BASE"; State
let moan _ = print_endline ". o O (Ugh! Damn moon! I am too old for this!)"; State  
let take_sample _ = print_endline "TAKING REGOLITH SAMPLE"; State
let empty_pockets _ = print_endline "EMPTYING POCKETS"; State
let end_day _ = print_endline "DAY ENDS"
