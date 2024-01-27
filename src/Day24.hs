{-# LANGUAGE DataKinds #-}

module Day24(day24) where

import Utils
import Linear.V4 ( V4(..) )
import Linear.V3 ( V3(..) )
import Linear.Matrix ( luSolveFinite )

parse3 :: String -> (Coord3, Coord3)
parse3 s = ((read $ p!!0, read $ tail $ p!!1, read $ tail $ p!!2), (read $ v!!0, read $ tail $ v!!1, read $ tail $ v!!2))
  where
    pieces = splitOn '@' s
    p = splitOn ',' (pieces!!0)
    v = splitOn ',' (pieces!!1)


parse2 :: String -> (Coord, Coord)
parse2 s = ((read $ c1!!0, read $ tail $ c1!!1), (read $ c2!!0, read $ tail $ c2!!1))
  where
    ps = splitOn '@' s
    c1 = splitOn ',' (ps!!0)
    c2 = splitOn ',' (ps!!1)


intercept :: (Coord, Coord) -> (Coord, Coord) -> Maybe (Double, Double)
intercept ((x1, y1), (v1, w1)) ((x2, y2), (v2, w2))
  | a<0 || b<0 = Nothing
  | otherwise = Just (fromIntegral x1 + a * fromIntegral v1, fromIntegral y1 + a * fromIntegral w1)
  where
    a, b :: Double
    b = fromIntegral (v1*(y1-y2)+w1*(x2-x1)) / fromIntegral (v1*w2-v2*w1)
    a = (fromIntegral (x2-x1) + b * fromIntegral v2) / fromIntegral v1


pairs ::[a] -> [(a,a)]
pairs [] = []
pairs [a] = []
pairs (a:as) = ((a,)<$>as) ++ pairs as


check :: (Double, Double) -> Bool
check (x,y) = x>=200000000000000 && x<=400000000000000 && y>=200000000000000 && y<=400000000000000


{-
I tried to use V6 with (p-pi) `cross` (v-vi) = 0
but Linear luSolveFinite would not work - I might revisit later...

So I used the fact that p-pi = t (v-vi)
With just 2 components we get 2 equations in 4 unknowns 
-}
solve :: [(Coord3, Coord3)] -> Rational
solve eqs = sum [xx,yy,zz]
  where
    (V4 xx yy _ _) = luSolveFinite m44 k4
    (V4 _ zz _ _)  = luSolveFinite n44 l4

    -- XY linear system
    m1,m2,m3,k4 :: V4 Rational
    m1 = V4 (_y $ v1-v0) (- (_x $ v1-v0)) (_y $ p1-p0) (- (_x $ p1-p0))
    m2 = V4 (_y $ v2-v0) (- (_x $ v2-v0)) (_y $ p2-p0) (- (_x $ p2-p0))
    m3 = V4 (_y $ v3-v0) (- (_x $ v3-v0)) (_y $ p3-p0) (- (_x $ p3-p0))
    m4 = V4 (_y $ v4-v0) (- (_x $ v4-v0)) (_y $ p4-p0) (- (_x $ p4-p0))
    
    m44 :: V4 (V4 Rational)
    m44 = V4 m1 m2 m3 m4
    k4 = V4 (_y p0 * _x v0 - _y v0 * _x p0 - _y p1 * _x v1 + _y v1 * _x p1)
            (_y p0 * _x v0 - _y v0 * _x p0 - _y p2 * _x v2 + _y v2 * _x p2)
            (_y p0 * _x v0 - _y v0 * _x p0 - _y p3 * _x v3 + _y v3 * _x p3)
            (_y p0 * _x v0 - _y v0 * _x p0 - _y p4 * _x v4 + _y v4 * _x p4)


    -- XZ linear system
    n1,n2,n3,l4 :: V4 Rational
    n1 = V4 (_z $ v1-v0) (- (_x $ v1-v0)) (_z $ p1-p0) (- (_x $ p1-p0))
    n2 = V4 (_z $ v2-v0) (- (_x $ v2-v0)) (_z $ p2-p0) (- (_x $ p2-p0))
    n3 = V4 (_z $ v3-v0) (- (_x $ v3-v0)) (_z $ p3-p0) (- (_x $ p3-p0))
    n4 = V4 (_z $ v4-v0) (- (_x $ v4-v0)) (_z $ p4-p0) (- (_x $ p4-p0))
    n44 :: V4 (V4 Rational)
    n44 = V4 n2 n1 n3 n4
    l4 = V4 (_z p0 * _x v0 - _z v0 * _x p0 - _z p2 * _x v2 + _z v2 * _x p2)
            (_z p0 * _x v0 - _z v0 * _x p0 - _z p1 * _x v1 + _z v1 * _x p1)
            (_z p0 * _x v0 - _z v0 * _x p0 - _z p3 * _x v3 + _z v3 * _x p3)
            (_z p0 * _x v0 - _z v0 * _x p0 - _z p4 * _x v4 + _z v4 * _x p4)

    p0, p1, p2, p3, p4, v0, v1, v2, v3, v4 :: V3 Rational
    p0 = V3 (hx 0) (hy 0) (hz 0)
    p1 = V3 (hx 1) (hy 1) (hz 1)
    p2 = V3 (hx 2) (hy 2) (hz 2)
    p3 = V3 (hx 3) (hy 3) (hz 3)
    p4 = V3 (hx 4) (hy 4) (hz 4)
    v0 = V3 (hdx 0) (hdy 0) (hdz 0)
    v1 = V3 (hdx 1) (hdy 1) (hdz 1)
    v2 = V3 (hdx 2) (hdy 2) (hdz 2)
    v3 = V3 (hdx 3) (hdy 3) (hdz 3)
    v4 = V3 (hdx 4) (hdy 4) (hdz 4)

    -- Get coordinates from a V3
    _x, _y, _z :: V3 a -> a
    _x (V3 x _ _) = x
    _y (V3 _ y _) = y
    _z (V3 _ _ z) = z

    -- Hailstone coordinates
    hx, hy, hz, hdx, hdy, hdz :: Int -> Rational
    hx i = fromIntegral x
      where
        ((x,_,_), (_,_,_)) = eqs !! i
    hy i = fromIntegral y
      where
        ((_,y,_), (_,_,_)) = eqs !! i
    hz i = fromIntegral z
      where
        ((_,_,z), (_,_,_)) = eqs !! i
    hdx i = fromIntegral dx
      where
        ((_,_,_), (dx,_,_)) = eqs !! i
    hdy i = fromIntegral dy
      where
        ((_,_,_), (_,dy,_)) = eqs !! i
    hdz i = fromIntegral dz
      where
        ((_,_,_), (_,_,dz)) = eqs !! i


day24 :: IO ()
day24 = do
  ss <- getLines 24
  let g2 = parse2 <$> ss
      eqns = parse3 <$> ss

  putStrLn $ "Day24: part1: " ++ show (length $ filter id $ mapMaybe ((check <$>) . uncurry intercept) (pairs g2))
  putStrLn $ "Day24: part2: " ++ show (solve eqns)
  return ()
