module Tensor where

-- Scalar x Vector
(.*|) :: Num a => a -> [a] -> [a]
a .*| xs = map (a *) xs 

(.+|) :: Num a => a -> [a] -> [a]
a .+| xs = map (a +) xs 

(.-|) :: Num a => a -> [a] -> [a]
a .-| xs = map (a -) xs 

-- Scalar x Matrix
(.*=) :: Num a => a -> [[a]] -> [[a]]
a .*= xss = map (a .*|) xss

(.+=) :: Num a => a -> [[a]] -> [[a]]
a .+= xss = map (a .+|) xss 

(.-=) :: Num a => a -> [[a]] -> [[a]]
a .-= xss = map (a .-|) xss 

-- Vector x Vector -> Vector
(|*|) :: Num a => [a] -> [a] -> [a]
xs |*| ys = zipWith (*) xs ys

(|+|) :: Num a => [a] -> [a] -> [a]
xs |+| ys = zipWith (+) xs ys

(|-|) :: Num a => [a] -> [a] -> [a]
xs |-| ys = zipWith (-) xs ys

-- Matrix * Matrix -> Matrix
(=*=) :: Num a => [[a]] -> [[a]] -> [[a]]
xss =*= wss = (flip map) xss (-*= wss)

(=+=) :: Num a => [[a]] -> [[a]] -> [[a]]
xss =+= wss = zipWith (|+|) xss wss

(=-=) :: Num a => [[a]] -> [[a]] -> [[a]]
xss =-= wss = zipWith (|-|) xss wss

-- Vector * Vector -> Scala
(-*|) :: Num a => [a] -> [a] -> a
xs -*| ys = sum $ zipWith (*) xs ys

-- Vector * Vector -> Matrix
(|*-) :: Num a => [a] -> [a] -> [[a]]
xs |*- ys = (flip map) xs (.*| ys) 

-- Matrix * Vector -> Vector
(=*|) :: Num a => [[a]] -> [a] -> [a]
wss =*| xs = (flip map) wss (-*| xs)

-- Vector * Matrix -> Matrix
(-*=) :: Num a => [a] -> [[a]] -> [a]
xs -*= wss = (flip map) (transpose wss) (-*| xs)

-- Matrix -> Matrix
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose wss    = map head wss : transpose (map tail wss)

-- Memo : Binary Operator !#$%&*+./<=>?@\^|-~:"

