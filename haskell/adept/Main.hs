module Main where

import Moonbase
import System.Random

typical_day () = do
  putStrLn "\nTypical day:";
  state <- beginDay ()
  state <- moan state
  state <- spacesuitOn state
  state <- openLock state
  state <- leaveBase state
  state <- closeLock state
  state <- takeSample state 
  state <- openLock state
  state <- enterBase state
  state <- closeLock state 
  state <- emptyPockets state
  state <- spacesuitOff state
  endDay state

does_not_feel_like_going_out () = do
  putStrLn "\nI dont feel like going out:";
  state <- beginDay ()
  state <- moan state
  state <- spacesuitOn state
  state <- openLock state
  state <- closeLock state
  state <- spacesuitOff state
  endDay state

taking_several_samples () = do
  putStrLn "\nTaking multiple samples:";
  state <- beginDay ()
  state <- moan state
  state <- spacesuitOn state
  state <- openLock state
  state <- leaveBase state
  state <- closeLock state
  state <- takeSample state
  state <- do
    -- Randomly decide to take a different sample
    rnd <- randomIO :: IO Float
    if rnd > 0.5
      then return state
      else do state <- emptyPockets state
              takeSample state
  state <- openLock state
  state <- enterBase state
  state <- closeLock state 
  state <- emptyPockets state
  state <- spacesuitOff state
  endDay state

two_shifts () = do
  putStrLn "\nTwo work shifts:";
  state <- beginDay ()
  state <- moan state
  state <- spacesuitOn state
  
  let work_shift state = do
        state <- openLock state
        state <- leaveBase state
        state <- closeLock state
        state <- takeSample state 
        state <- openLock state
        state <- enterBase state
        state <- closeLock state 
        state <- emptyPockets state
        return state
 
  state <- work_shift state
  state <- spacesuitOff state
  putStrLn "First shift ended, second begins";
  state <- moan state
  state <- spacesuitOn state
  state <- work_shift state
  state <- spacesuitOff state
  endDay state

-- Example usage
main :: IO ()
main = do
  typical_day ();
  does_not_feel_like_going_out ();
  taking_several_samples ();
  two_shifts ()
