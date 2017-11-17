--
-- Tuesday, October 19, 2017
--
-- module Main where

import Data.Char
import Control.Exception
import System.IO

{-
   Until now we have used Haskell operators that could fail and
   throw exceptions.

   Today we will look at how we can use data definitions to program
   safe versions of functions and operations that behave as we want them
   to in undefined application modes ...
-}

{-
   Some examples using Monad Maybe:

   *Main> :type Just
   Just :: a -> Maybe a

   *Main> :type Nothing
   Nothing :: Maybe a

   data Maybe a = Nothing | Just a 	-- Defined in `Data.Maybe'
   instance Eq a => Eq (Maybe a) -- Defined in `Data.Maybe'
   instance Monad Maybe -- Defined in `Data.Maybe'
   instance Functor Maybe -- Defined in `Data.Maybe'
   instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'
   instance Read a => Read (Maybe a) -- Defined in `GHC.Read'
   instance Show a => Show (Maybe a) -- Defined in `GHC.Show'

  *Main> :type (>>)
  (>>) :: Monad m => m a -> m b -> m b

  *Main> :type (>>=)
  (>>=) :: Monad m => m a -> (a -> m b) -> m b

-}


a = (Just 5.6) :: Maybe Float
b = (Nothing) :: Maybe Float
c = (Just 19.6) :: Maybe Float
d = Just (-16.0) :: Maybe Float

safeSqrt1 :: Float -> Maybe Float
safeSqrt1 n	| n < 0		=	Nothing
            	| otherwise	=	Just (sqrt n)

sq0 = sqrt (-1)
sq1 = safeSqrt1 (-1)
sq2 = safeSqrt1 (9)

safeDiv1 :: Float -> Float -> Maybe Float
safeDiv1 m n 	| n == 0	= Nothing
		| otherwise	= Just (m/n)

dv0 = 10 / 0
dv1 = safeDiv1 10 0

-- What if I want:
-- 	i) 	sqrt ( sqrt ( 16.0 ) )
-- 	ii) 	sqrt ( a / b) 
-- 	iii) 	sqrt a / sqrt b 

safeSqrt :: Maybe Float -> Maybe Float
safeSqrt Nothing	= Nothing
safeSqrt (Just n) 	| n < 0		=	Nothing
            		| otherwise	=	Just (sqrt n)

safeDiv :: Maybe Float -> Maybe Float -> Maybe Float
safeDiv Nothing _ = Nothing
safeDiv _ Nothing = Nothing
safeDiv (Just m) (Just n) = safeDiv1 m n

f0 = safeSqrt ( safeSqrt a )
f1 = safeSqrt ( safeDiv a b) 
f2 = safeSqrt ( safeDiv a c) 

--
-- Maybe can can be used to handle undefined application values
--
myConv :: Maybe Int -> Maybe Char
myConv Nothing = Nothing
myConv (Just i) = if (i >= 0) then Just (chr i) else Nothing

mc0 = myConv (Just 100)
mc1 = myConv (Just 10)
mc2 = myConv (Just 1000)

-- We can now report when an illegal argument is applied
mc3 = myConv (Just (-1)) 

--
-- Maybe can take any value, so here's another example 
--
myConvert :: Maybe Int -> Maybe Float
myConvert Nothing = Nothing
myConvert (Just i) = Just (fromIntegral i)

mc4 = myConvert (Just 100)

--
-- If we want to define a safe sign function:
-- 
mySgn :: Float -> Maybe Float
mySgn x	| x < 0.0		= Just (-1.0)
	| x > 0.0		= Just 1.0
	| otherwise	= Nothing

safeApply :: Maybe Float -> (Float -> Maybe Float) -> Maybe Float
safeApply Nothing _ = Nothing
safeApply (Just v) sfx = case (sfx v) of
				Nothing -> Nothing
				(Just r) -> (Just r)

-- There is also a function which takes a Float and turns it into a Maybe Float
-- f v = Just v

g1   =  safeApply (myConvert (Just 7)) mySgn
g2   =  safeApply (safeApply a mySgn) safeSqrt1 
g3   =  safeApply (safeApply d mySgn) safeSqrt1 
g3'  =  a `safeApply` safeSqrt1
g3'' =  (d `safeApply` mySgn) `safeApply` safeSqrt1

g4 =  d `safeApply` 
      mySgn `safeApply` 
      safeSqrt1 `safeApply` 
      (\x -> Just (x * 99))

{-
(>>=) =  safeApply 
-}

m0 = d >>= mySgn
 
g4' =  d >>=
      mySgn >>=
      safeSqrt1 >>= \x -> 
      Just (x * 99)

g5 = (safeDiv (safeSqrt a) c) >>= 
	mySgn >>= 
 	(\x -> Just (x*99))

g5' = do 
	u <- safeDiv (safeSqrt a) c
	v <- mySgn u
	Just (v*99)

myGetLine :: IO String
myGetLine = do	x <- getChar
		if x == '\n' then
			return []
		else
		  do 	xs <- myGetLine
			return (x:xs)	

myGetLineLen :: IO Int
myGetLineLen = do	x <- getChar
			if x == '\n' then
				return 0
			else
			  do 	xs <- myGetLineLen
				return (1 + xs)	

myGetLineLen' :: IO Int
myGetLineLen' = do	x <- myGetLine
			return (length x)


--
-- This is unsafe because the function maps two different values
-- in this case (Just 0.0) and Nothing to 0.0
-- As a result you would not be able to get the inverse value back,
-- or in other words, you would never know if your 0.0 result came
-- from Nothing or (Just 0.0)
--
unsafeMaybeFloatToFloat :: Maybe Float -> Float
unsafeMaybeFloatToFloat (Just f) = f
unsafeMaybeFloatToFloat (Nothing) = 0.0

us0 = unsafeMaybeFloatToFloat (Just 0.0)
us1 = unsafeMaybeFloatToFloat (Nothing)
