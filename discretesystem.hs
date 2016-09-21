data Bit = On | Off deriving (Show)

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

map' :: [[Bit] -> Bit] -> [Bit] -> [Bit]
map' _            ([])     = [] 
map' ([])         _        = []
map' (f : bitFns) (bitVec) = let
                               newBit = f bitVec
                             in
                               newBit : (map' bitFns bitVec)


timeStep :: Int -> [[Bit] -> Bit] -> [Bit] -> [Bit]
timeStep (0) (_)    bitVec = bitVec
timeStep n   bitFns bitVec = timeStep (n-1) bitFns (map' bitFns bitVec)

funcarray = [\b -> and' (b !! 0) (b !! 1),
             \b -> not' (b !! 2),
             \b -> and' (b !! 0) (b !! 1)]

run' :: Int -> [Bit]
run' steps = timeStep steps funcarray [On, On, Off]
