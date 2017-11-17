-- CSCE 314-599: Homework 5
-- Module: MeasureWater
-- Oneal Abdulrahim
-- Due: Thursday, October 19, 2017 @ 11:59p

{-
Provides an indication whether the given jug values can produce desired value.
@param  x    the capacity, in gallons, of the first jug of water
        y    the capacity, in gallons, of the second jug of water
        z    the desired amount of gallons
@return Bool Whether solution for Bézout's identity for given inputs exists,
             ax + by = z
            
-}
measureWater :: Int -> Int -> Int -> Bool
measureWater x y z | z > x + y  = False -- impossible case, False
                   | x + y == z = True -- if we already have enough, True
                   | x == z     = True -- ""
                   | y == z     = True -- ""
                   | otherwise  = z `mod` gcd x y == 0
      -- every integer of the form ax + by is a multiple of the gcd, z