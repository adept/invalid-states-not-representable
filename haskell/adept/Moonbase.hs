{-# LANGUAGE DataKinds #-}

module Moonbase where

data LockState = Closed | Open
data SuitState = On | Off
data Location = Inside | Outside
data Pockets = Empty | Full

-- Define the state type with four phantom type parameters that track our state.
-- State itself holds no data
data State (lock :: LockState) (suit :: SuitState) (location :: Location) (pockets :: Pockets) where
    State :: State lock suit location pockets

beginDay :: () -> IO (State 'Closed 'Off 'Inside 'Empty)
beginDay () = do
    putStrLn "DAY BEGINS"
    return State

openLock :: State 'Closed 'On location pockets -> IO (State 'Open 'On location pockets)
openLock _ = do
    putStrLn "OPENING LOCK"
    return State

closeLock :: State 'Open 'On location pockets -> IO (State 'Closed 'On location pockets)
closeLock _ = do
    putStrLn "CLOSING LOCK"
    return State

spacesuitOn :: State 'Closed 'Off 'Inside 'Empty -> IO (State 'Closed 'On 'Inside 'Empty)
spacesuitOn _ = do
    putStrLn "SPACESUIT ON"
    return State

spacesuitOff :: State 'Closed 'On 'Inside 'Empty -> IO (State 'Closed 'Off 'Inside 'Empty)
spacesuitOff _ = do
    putStrLn "SPACESUIT OFF"
    return State

leaveBase :: State 'Open 'On 'Inside pockets -> IO (State 'Open 'On 'Outside pockets)
leaveBase _ = do
    putStrLn "LEAVING BASE"
    return State

enterBase :: State 'Open 'On 'Outside 'Full -> IO (State 'Open 'On 'Inside 'Full)
enterBase _ = do
    putStrLn "ENTERING BASE"
    return State

moan :: State lock 'Off location pockets -> IO (State lock 'Off location pockets)
moan _ = do
    putStrLn ". o O (Ugh! Damn moon! I am too old for this!)"
    return State

takeSample :: State 'Closed 'On 'Outside 'Empty -> IO (State 'Closed 'On 'Outside 'Full)
takeSample _ = do
    putStrLn "MINING REGOLITH SAMPLE"
    return State

emptyPockets :: State lock suit location 'Full -> IO (State lock suit location 'Empty)
emptyPockets _ = do
    putStrLn "EMPTYING POCKETS"
    return State

endDay :: State 'Closed 'Off 'Inside 'Empty -> IO ()
endDay _ = do
    putStrLn "DAY ENDS"

