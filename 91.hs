type Pos = (Integer,Integer)

applyVector :: Pos -> Pos -> Pos
applyVector (px,py) (pa,pb) = (px + pa, py + pb)

deletePos :: Pos -> [Pos] -> [Pos]
deletePos p [] = []
deletePos p (tp:tps) = if p == tp then tps else tp : deletePos p tps

cMap :: [Pos]
cMap = [(i,k) | i <- [1..8], k <- [1..8]]

nightVector = [(expp * p, expq * q) | p <- [1,2], q <- [1,2], p /= q,
                                      expp <- [-1,1], expq <- [-1,1]]

attackArea :: Pos -> [Pos] -> [Pos]
attackArea p rest = let posibility = map (applyVector p) nightVector
                    in  filter areaFilter posibility
                    where areaFilter p = if p `elem` cMap && p `elem` rest then True
                                         else False

restArea :: [Pos] -> [Pos] -> Pos -> ([Pos], [Pos])
restArea rest used p = (rest', p:used)
    where rest' = deletePos p rest

knightTour :: [[Pos]] -> Int -> Pos -> ([Pos], [Pos]) -> [[Pos]]
knightTour acc n p ([],   _)    = acc
knightTour acc n p (rest, used) = let as = (attackArea p rest)
                                  in if   n  > 63 then (map (p:) acc) else
                                      if   as == [] then []
                                      else
                                           if   acc == [] then let acc'   = map (p:) [[]]
                                                                   rest'  = deletePos p rest
                                                                   used'  = p:used
                                                                   rs     = map (restArea rest' used') as
                                                               in  concat $ zipWith (knightTour acc' (n+1)) as rs
                                           else let acc'   = map (p:) acc
                                                    rest'  = deletePos p rest
                                                    used'  = p:used
                                                    rs     = map (restArea rest' used') as
                                                in  concat $ zipWith (knightTour acc' (n+1)) as rs
