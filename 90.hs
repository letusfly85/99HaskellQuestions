type Pos = (Integer,Integer)

cMap :: [Pos]
cMap = [(i,k) | i <- [1..8], k <- [1..8]]

attackArea :: Pos -> [Pos] -> [Pos]
attackArea (px,py) used = let h = filter strict [(px + i, py    ) | i <- [-8..8]]
                              v = filter strict [(px    , py + i) | i <- [-8..8]]
                              r = filter strict [(px + i, py + i) | i <- [-8..8]]
                              l = filter strict [(px + i, py - i) | i <- [-8..8]]
                          in  (px,py):(h ++ v ++ r ++ l)
    where strict (x, y) = if x > 0 && x < 9 && y > 0 && y < 9 &&
                             not ((x,y) `elem` used) && ((px,py) /= (x,y))        then
                              True
                          else
                              False

restArea :: Pos -> [Pos] -> [Pos] -> (Pos,[Pos],[Pos])
restArea p rest used = let a = attackArea p used
                       in  (p,restArea' rest a, a ++ used)
    where restArea' [] a = []
          restArea' (c:cs) a = if (c `elem` a) || c == p then restArea' cs a
                               else c: (restArea' cs a)

recursive :: [[Pos]] -> [Pos] -> [Pos] -> Pos -> [[Pos]]
recursive acc r u p = recursive' [[]] [1..8] r u p 
    where recursive' acc []  _  _    _      = acc
          recursive' acc [8]    rest used p = map (p:) acc
          recursive' acc (n:ns) rest used p = let (r0, newRest , newUsed) = restArea p rest used 
                                                  newerRest = takeList (n+1) newRest
                                              in  concat $ map (\r -> recursive' (map (p:) acc) ns newRest newUsed r) (newerRest)

(p,r,u)    = restArea (2,1) cMap []
(p0,r0,u0) = restArea (7,2) r u
(p1,r1,u1) = restArea (4,3) r0 u0
(p2,r2,u2) = restArea (8,4) r1 u1
(p3,r3,u3) = restArea (1,5) r2 u2
(p4,r4,u4) = restArea (4,6) r3 u3
(p5,r5,u5) = restArea (5,7) r4 u4

takeList :: Integer -> [Pos] -> [Pos]
takeList n [] = []
takeList n (p:ps) = if n == snd p then p: takeList n ps
                    else takeList n ps

queens :: Int -> [[Int]]
queens n = filter test (generate n)
    where generate 0 = [[]]
          generate k = [q:qs | q <- [1..n], qs <- generate (k-1)]
          test [] = True
          test (q:qs) = isSafe q qs && test qs
          isSafe try qs = not (try `elem` qs || sameDiag try qs)
          sameDiag try qs =  any (\(colDist, q) ->  abs (try - q) == colDist) $ zip [1..] qs
