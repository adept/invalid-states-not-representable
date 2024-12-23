let _we_already_did_it () =
  let open Moonbase in
  let state = begin_day () in
  let state = spacesuit_on state in
  let state = spacesuit_on state (* already on *) in
  let state = open_lock state in
  let state = open_lock state (* already open *) in
  let state = leave_base state in
  let state = leave_base state (* already left *) in
  let state = close_lock state in
  let state = close_lock state (* already closed *) in
  let state = take_sample state in 
  let state = take_sample state (* pockets full *) in 
  let state = open_lock state in
  let state = open_lock state (* already open *) in
  let state = enter_base state in
  let state = enter_base state (* already inside *) in
  let state = close_lock state in 
  let state = close_lock state (* already closed *) in 
  let state = empty_pockets state in
  let state = empty_pockets state (* already empty *) in
  let state = spacesuit_off state in
  let state = spacesuit_off state (* already off *) in
  end_day state

let in_space_nobody_can_hear_you_scream () =
  let open Moonbase in
  let state = begin_day () in
  let state = moan state in
  let state = spacesuit_on state in
  let state = open_lock state in
  let state = leave_base state in
  let state = close_lock state in
  (* Cannot take spacesuit off, will suffocate *)
  let state = spacesuit_off state in
  end_day state

let _taking_several_samples_or_not () =
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
    else empty_pockets state
  in
  (* At this point we randomly either have a sample, or not *)
  let state = open_lock state in
  let state = enter_base state in
  let state = close_lock state in 
  let state = empty_pockets state in
  let state = spacesuit_off state in
  end_day state

let _two_shifts_went_bad () =
  let open Moonbase in
  print_endline "\nTwo work shifts:";
  let state = begin_day () in
  let state = moan state in
  let state = spacesuit_on state in
  let state = open_lock state in
  let work_shift state = 
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
  let state = (* at this point the lock is closed, so we can't leave *) work_shift state in
  let state = spacesuit_off state in
  end_day state

let () = print_endline "No examples in this file should compile"
