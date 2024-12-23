(* State has four phantom type parameters that track our current state. They
   exist during compile time only and erased afterwards. State does not have 
   any internal data whatsoever (but it could, if the task demands it - for example,
   we can track the number of samples mined) *)

type ('lock, 'suit, 'location, 'pockets) state 

val begin_day : unit -> ([`lock_closed], [`spacesuit_off], [`inside], [`empty]) state

val open_lock  :([`lock_closed],[`spacesuit_on],'location, 'pockets) state -> ([`lock_open],[`spacesuit_on],'location,'pockets) state
val close_lock : ([`lock_open],[`spacesuit_on],'location, 'pockets) state -> ([`lock_closed],[`spacesuit_on],'location, 'pockets) state

val spacesuit_on : ([`lock_closed],[`spacesuit_off], [`inside], [`empty]) state -> ([`lock_closed],[`spacesuit_on],[`inside],[`empty]) state
val spacesuit_off : ([`lock_closed],[`spacesuit_on],[`inside], [`empty]) state -> ([`lock_closed],[`spacesuit_off],[`inside],[`empty]) state

val leave_base : ([`lock_open],[`spacesuit_on], [`inside], 'pockets) state -> ([`lock_open],[`spacesuit_on],[`outside], 'pockets) state 
val enter_base : ([`lock_open],[`spacesuit_on], [`outside],[`full]) state -> ([`lock_open],[`spacesuit_on],[`inside],[`full]) state

val moan : ('lock,[`spacesuit_off], 'location, 'pockets) state -> ('lock,[`spacesuit_off],'location, 'pockets) state

val take_sample : ([`lock_closed],[`spacesuit_on],[`outside],[`empty]) state -> ([`lock_closed],[`spacesuit_on],[`outside],[`full]) state
val empty_pockets : ('lock,'suit, 'location,[`full]) state -> ('lock,'suit,'location,[`empty]) state

val end_day : ([`lock_closed], [`spacesuit_off], [`inside], [`empty]) state -> unit
