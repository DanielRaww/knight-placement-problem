-- Daniel Raw, Abbie Daniel
-- CSCI312

knightPlace list = out
  where
    x = solve list
    out = zeroL (length x -1) x

--this function scans every position and replaces the empty list into a list with 0.
zeroL list x
  | list == -1 = []
  | [x!!list] == [[]] = zeroL(list-1) x++ [[0]]
  | otherwise = zeroL(list-1) x++ [x!!list]

solve list = check list
  where
    check [] = []
  --recursive checks each element in each column to see if it is safe from the queen
    check(x:xs) = [ [ a|a <- [1..(length list)], safePos x xs a ] ] ++ check xs
  -- if a position fulfills the safe requirements, then the position is not safe (not (scan x xs a n))
    safePos x xs a = and [ not (scan x xs a n) | n <- [0..(length list)-1 ] ]
    scan x xs a n
      | x /= 0 = True --when the position is not 0
      | a == list!!n && list!!n /= 0 = True --when the cell is not 0 and is equal to any number between 1 and the length of the list
      --checks to see if the knight is diagonal to the queen
      | xs == [] && list!!n /= 0 && abs((length list) - n - 1) == abs(a - list!!n) = True
      | xs /= [] && list!!n /= 0 && abs((length list) - (length xs) - n - 1) == abs(a - list!!n) = True
      --checks to see if the queen is safe from the knight
      | xs == [] && list!!n /= 0 && abs(a - list!!n) + abs((length list) - n - 1) == 3 = True
      | xs /= [] && list!!n /= 0 && abs(a - list!!n) + abs((length list) - (length xs) - n - 1) == 3 = True
      | otherwise = False




      -- knightsPlace [Int] --> [[Int]]
      --knightPlace list = if list ==[] then [] else [x|x <- knightlist]
      --  where
      --  knightlist = [[x|x <- [1..(length list)],  [i|i <- [0..(length list)-1]], safe x list i] ]
      --  safe x list i = (safeKnight x list i) && (safeQueen x list i)
      --  safeKnight x list i = list!!i == 0 && and[not(checks x list i a) | a <- [0..(length list)-1]]
      --  checks x list i a = x == list!!a || (list!!a /= 0 && abs(a-i) == abs(list!!a-x))
      --  safeQueen x list i = and[not(checksQ x list i a) | a <- [0..(length list)-1]]
