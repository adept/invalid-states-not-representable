let typical_day () =
  let open Moonbase in
  print_endline "\nTypical day:";
  let state = begin_day () in
  let state = moan state in
  let state = spacesuit_on state in
  let state = open_lock state in
  let state = leave_base state in
  let state = close_lock state in
  let state = take_sample state in 
  let state = open_lock state in
  let state = enter_base state in
  let state = close_lock state in 
  let state = empty_pockets state in
  let state = spacesuit_off state in
  end_day state

let does_not_feel_like_going_out () =
  let open Moonbase in
  print_endline "\nI dont feel like going out:";
  let state = begin_day () in
  let state = moan state in
  let state = spacesuit_on state in
  let state = open_lock state in
  let state = close_lock state in
  let state = spacesuit_off state in
  end_day state

let taking_several_samples () =
  let open Moonbase in
  print_endline "\nTaking multiple samples:";
  let state = begin_day () in
  let state = moan state in
  let state = spacesuit_on state in
  let state = open_lock state in
  let state = leave_base state in
  let state = close_lock state in
  let state = take_sample state in
  let state =
    (* Randomly decide to take a different sample *)
    Random.self_init();
    if Random.float 1.0 > 0.5 then state
    else let state = empty_pockets state in take_sample state
  in
  let state = open_lock state in
  let state = enter_base state in
  let state = close_lock state in 
  let state = empty_pockets state in
  let state = spacesuit_off state in
  end_day state

let two_shifts () =
  let open Moonbase in
  print_endline "\nTwo work shifts:";
  let state = begin_day () in
  let state = moan state in
  let state = spacesuit_on state in
  let work_shift state = 
    let state = open_lock state in
    let state = leave_base state in
    let state = close_lock state in
    let state = take_sample state in 
    let state = open_lock state in
    let state = enter_base state in
    let state = close_lock state in 
    let state = empty_pockets state in
    state
  in
  let state = work_shift state in
  let state = spacesuit_off state in
  print_endline "First shift ended, second begins";
  let state = moan state in
  let state = spacesuit_on state in  
  let state = work_shift state in
  let state = spacesuit_off state in
  end_day state

let () =
  typical_day ();
  does_not_feel_like_going_out();
  taking_several_samples();
  two_shifts()
