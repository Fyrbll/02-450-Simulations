-- datatype for values that can be either 
-- On (like True) or Off (like False) 
data Bit = On | Off deriving (Show)

-- defining functions and', or', not' and xor'
-- which are analogs of the and, or, not and xor
-- logical operators
and' :: Bit -> Bit -> Bit
and' (On) (On) = On
and' _    _    = Off

or' :: Bit -> Bit -> Bit
or' (On) _    = On
or' _    (On) = On
or' _    _    = Off

not' :: Bit -> Bit
not' (On) = Off
not' _    = On

xor' :: Bit -> Bit -> Bit
xor' (On)  (On)  = Off
xor' (Off) (Off) = Off
xor' _     _     = On

-- takes a list of [Bit] -> Bit functions, call it bitFns
-- and a [Bit], call it bitVec
-- and generates a new [Bit], call it bitVec'
-- where the ith element of bitVec' is the result
-- of applying the ith element of bitFns to bitVec
-- notice that bitVec' is a synchronous update of bitVec
map' :: [[Bit] -> Bit] -> [Bit] -> [Bit]
map' _            ([])     = [] 
map' ([])         _        = []
map' (f : bitFns) (bitVec) = let
                               newBit = f bitVec
                             in
                               newBit : (map' bitFns bitVec)

-- simulates the evolution of a system after n steps in time
-- given the starting configuation bitVec and the list of
-- update functions bitFns
-- the ith element of bitFns should correspond to the
-- update function for the ith element of bitVec
timeStep :: Int -> [[Bit] -> Bit] -> [Bit] -> [Bit]
timeStep (0) (_)    bitVec = bitVec
timeStep n   bitFns bitVec = timeStep (n-1) bitFns (map' bitFns bitVec)

-- Now, suppose we want to represent the simple system seen in
-- slide 46 of the notes from Lecture 7 (https://goo.gl/B4Z6zt):
-- this is the list of functions that the model uses
funcarray = [\b -> and' (b !! 0) (b !! 1),
             \b -> not' (b !! 2),
             \b -> and' (b !! 0) (b !! 1)]
-- one can load the module in GHC and call
-- run' 0
-- to the the initial state [On, On, Off]
-- run' t
-- to view the state after t time steps
-- and notice that the state doesn't change
-- after step 5
run' :: Int -> [Bit]
run' steps = timeStep steps funcarray [On, On, Off]
