// Try it in https://play.rust-lang.org/
use rand::Rng;
use std::marker::PhantomData;

struct Closed;
struct Open;

struct On;
struct Off;

struct Inside;
struct Outside;

struct Empty;
struct Full;

struct State<Lock, Suit, Location, Pockets> {
    _lock: PhantomData<Lock>,
    _suit: PhantomData<Suit>,
    _location: PhantomData<Location>,
    _pockets: PhantomData<Pockets>,
}

fn new_state<Lock, Suit, Location, Pockets>() -> State<Lock, Suit, Location, Pockets> {
    State {
        _lock: PhantomData,
        _suit: PhantomData,
        _location: PhantomData,
        _pockets: PhantomData,
    }
}

fn begin_day() -> State<Closed, Off, Inside, Empty> {
    println!("DAY BEGINS");
    new_state()
}

fn open_lock<Loc, Pockets>(_: State<Closed, On, Loc, Pockets>) -> State<Open, On, Loc, Pockets> {
    println!("OPENING LOCK");
    new_state()
}

fn close_lock<Loc, Pockets>(_: State<Open, On, Loc, Pockets>) -> State<Closed, On, Loc, Pockets> {
    println!("CLOSING LOCK");
    new_state()
}

fn spacesuit_on(_: State<Closed, Off, Inside, Empty>) -> State<Closed, On, Inside, Empty> {
    println!("SPACESUIT ON");
    new_state()
}

fn spacesuit_off(_: State<Closed, On, Inside, Empty>) -> State<Closed, Off, Inside, Empty> {
    println!("SPACESUIT OFF");
    new_state()
}

fn leave_base<Pockets>(_: State<Open, On, Inside, Pockets>) -> State<Open, On, Outside, Pockets> {
    println!("LEAVING BASE");
    new_state()
}

fn enter_base(_: State<Open, On, Outside, Full>) -> State<Open, On, Inside, Full> {
    println!("ENTERING BASE");
    new_state()
}

fn moan<Lock, Location, Pockets>(
    _: State<Lock, Off, Location, Pockets>,
) -> State<Lock, Off, Location, Pockets> {
    println!(". o O (Ugh! Damn moon! I am too old for this!)");
    new_state()
}

fn take_sample(_: State<Closed, On, Outside, Empty>) -> State<Closed, On, Outside, Full> {
    println!("TAKING REGOLITH SAMPLE");
    new_state()
}

fn empty_pockets<Lock, Suit, Location>(
    _: State<Lock, Suit, Location, Full>,
) -> State<Lock, Suit, Location, Empty> {
    println!("EMPTYING POCKETS");
    new_state()
}

fn end_day(_: State<Closed, Off, Inside, Empty>) {
    println!("DAY ENDS");
}

fn typical_day() {
    println!("\nTypical day:");
    let state = begin_day();
    let state = moan(state);
    let state = spacesuit_on(state);
    let state = open_lock(state);
    let state = leave_base(state);
    let state = close_lock(state);
    let state = take_sample(state);
    let state = open_lock(state);
    let state = enter_base(state);
    let state = close_lock(state);
    let state = empty_pockets(state);
    let state = spacesuit_off(state);
    end_day(state);
}

fn does_not_feel_like_going_out() {
    println!("\nI dont feel like going out:");
    let state = begin_day();
    let state = moan(state);
    let state = spacesuit_on(state);
    let state = open_lock(state);
    let state = close_lock(state);
    let state = spacesuit_off(state);
    end_day(state)
}

fn taking_several_samples() {
    println!("\nTaking multiple samples:");
    let state = begin_day();
    let state = moan(state);
    let state = spacesuit_on(state);
    let state = open_lock(state);
    let state = leave_base(state);
    let state = close_lock(state);
    let state = take_sample(state);
    let state = {
        let mut rng = rand::thread_rng();
        if rng.gen::<f64>() > 0.5 {
            state
        } else {
            let state = empty_pockets(state);
            take_sample(state)
        }
    };
    let state = open_lock(state);
    let state = enter_base(state);
    let state = close_lock(state);
    let state = empty_pockets(state);
    let state = spacesuit_off(state);
    end_day(state)
}

fn main() {
    typical_day();
    does_not_feel_like_going_out();
    taking_several_samples()
}
