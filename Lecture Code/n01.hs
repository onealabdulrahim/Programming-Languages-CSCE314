--
-- IO is clumpsy
--

x = 3
y = 15;
whileHaskell x y =
   if (y < 100) then
      (wstr ++ (whileHaskell x' y''))
   else
      ("END: x = " ++ (show x) ++ ", y = " ++ (show y) ++ "\n")
   where
      x'   = x + y + 5
      y'   = if (x' < 50) then (y + x' + 5) else y
      y''  = y' + 10
      wstr =
        "At while end: x = " ++ (show x') ++ ", y = " ++ (show y'') ++ "\n"

whileJava = putStr (whileHaskell x y)

{-
   At while end: x = 23, y = 53
   At while end: x = 81, y = 63
   At while end: x = 149, y = 73
   At while end: x = 227, y = 83
   At while end: x = 315, y = 93
   At while end: x = 413, y = 103
   END: x = 413, y = 103
-}

--
-- But calculus/math is much easier:
--
whileHaskellN x y =
   if (y < 100) then
      (wnum : (whileHaskellN x' y''))
   else
      [(x, y)]
   where
      x'   = x + y + 5
      y'   = if (x' < 50) then (y + x' + 5) else y
      y''  = y' + 10
      wnum = (x', y'')
--
-- [(23,53),(81,63),(149,73),(227,83),(315,93),(413,103),(413,103)]
--