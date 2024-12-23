# Examples of making invalid states non-representable (at compile time)

This repository contains examples of making invalid states non-representable (at compile time), using the following toy problem

## The imaginary domain in which our story unfolds

Imagine a small base on the Moon with an astronaut living in it. They could put on or take off their space suite, open the lock doors or shut them, leave the base and enter it (come back), and while they are out, they could take a regolith sample, which goes into a special pocket on the suit. The pocket could later be emptied. They could also moan about their life when nobody could hear them.

We model this as a module/library/class/compilation unit (as suitable for your language of choice) called `Moonbase` that implements the following functions (choose camelCased or under_scored identifiers as you see fit):
- `begin_day` -- starts a new day, with an astronaut inside, without a suit, and with the lock closed. It could be a class constructor or a similar initialization mechanism.
- `open_lock`
- `close_lock`
- `spacesuit_on`
- `spacesuit_off`
- `leave_base`
- `enter_base`
- `moan`
- `take_sample`
- `empty_pocket`
- `end_day` -- end the day on the Moonbase. It could be a class destructor or a similar wind-down mechanism.

For simplicity, these functions would print out a message to stdout, and do nothing else. In terms of behaviour, this Python implementation is sufficient and complete:

<details>
  <summary>Simplest Python implementation</summary>
  
```python
class Moonbase:
  def __init__(self):
    print ("DAY BEGINS")
  def open_lock(self):
    print ("OPENING LOCK")
  def close_lock(self):
    print ("CLOSING LOCK")
  def spacesuit_on(self):
    print ("SPACESUIT ON")
  def spacesuit_off(self):
    print ("SPACESUIT OFF")
  def leave_base(self):
    print ("LEAVING BASE")
  def enter_base(self):
    print ("ENTERING BASE")
  def moan(self):
    print (". o O (Ugh! Damn moon! I am too old for this!)")  
  def take_sample(self):
    print ("MINING REGOLITH")
  def empty_pocket(self):
    print ("EMPTYING POCKET")
  def end_day(self):
    print ("DAY ENDS")
```
</details>

If you want these functions to take additional arguments or return some values, you are free to do this - do whatever is idiomatic/natural in your chosen language.

### Additional Rules

Additionally, there are a bunch of rules that we want to enforce (at compile time!) which determine when a particular function can or cannot be called.

1. You can only put the suit on if it is not already on
2. You can only remove the suit if it is already on
3. You can open the lock only if it is closed
4. You can close the lock only if it is open
5. You have to have the suit on to open the lock
6. Suit pockets should be empty to put it on (otherwise regolith sample can puncture the suit)
7. You cannot take the suit off while outside
8. You cannot take the suit off when the lock is open
9. You cannot take the suit off when its pockets are full (otherwise regolith sample can puncture the suit)
10. You can only leave base when you are inside, with the suit on, and with the lock open
11. If you go outside, you always need to take a regolith sample. So you can only enter the base with the suit's pocket full and lock open
12. The astronaut should never moan about life on the moon when in the suit - it will be recorded and will reflect poorly on them
13. Taking regolith samples could only be done outside. It creates a lot of dust, so it should only be done with the lock closed. The suit pocket should be empty
14. Suit pocket could be emptied whenever. If done outside, it means that the astronaut is now without a sample, and might need to procure one to re-enter the base
15. At the end of the day, the astronaut should be inside the base, with the suit off, and the lock closed

We can add some extra state to the naive Python implementation above and use it to enforce those rules (at runtime):

<details>
<summary>Python implementation that enforces rules via assert()</summary>  
  
```python
class Moonbase:
  lockOpen = False
  suitOn  = False
  isOutside = False
  pocketsFull = False
  
  def __init__(self):
    print ("DAY BEGINS")
  def open_lock(self):
    assert(not self.lockOpen)
    assert(self.suitOn)
    print ("OPENING LOCK")
    self.lockOpen=True
  def close_lock(self):
    assert(self.lockOpen)
    print ("CLOSING LOCK")
    self.lockOpen=False
  def spacesuit_on(self):
    assert(not self.suitOn)
    assert(not self.pocketsFull)
    print ("SPACESUIT ON")
    self.suitOn=True
  def spacesuit_off(self):
    assert(self.suitOn)
    assert(not self.isOutside)
    assert(not self.lockOpen)
    assert(not self.pocketsFull)
    print ("SPACESUIT OFF")
    self.suitOn=False
  def leave_base(self):
    assert(self.lockOpen)
    assert(self.suitOn)
    assert(not self.isOutside)
    print ("LEAVING BASE")
    self.isOutside=True
  def enter_base(self):
    assert(self.isOutside)
    assert(self.pocketsFull)
    assert(self.lockOpen)
    print ("ENTERING BASE")
    self.isOutside=False
  def moan(self):
    assert(not self.suitOn)
    print (". o O (Ugh! Damn moon! I am too old for this!)")  
  def take_sample(self):
    assert(self.isOutside)
    assert(not self.lockOpen)
    assert(not self.pocketsFull)
    print ("TAKING REGOLITH SAMPLE")
    self.pocketsFull=True
  def empty_pockets(self):
    assert(self.pocketsFull)
    print ("EMPTYING POCKETS")
    self.pocketsFull=False
  def end_day(self):
    assert(not self.isOutside)
    assert(not self.lockOpen)
    assert(not self.suitOn)
    assert(not self.pocketsFull)
    print ("DAY ENDS")
```
</details>

# The Problem

Everything on the Moon is expensive, including runtime errors. Can we enforce the rules entirely at the compile time, so that the program that violates one of the rules above simply cannot be written?

Here are the implementations in:
- OCaml: 1
- Haskell: 1

# Contribute implementations!

Can you contribute an implementation in your favourite language? If you can, please do! If we already have your favourite language covered, but you feel that you can do better, please contribute alternative implementation.

Fork this project, create `<language>/<your github id>/`, place your implementation there, and submit a PR.

# Should a particular program compile or not?

You can treat OCaml implementation in `ocaml/adept` as a reference implementation, and
test against it.
