module Stream where

import Prelude ((+), (-), (<), (++), otherwise, id)
import qualified Prelude as P

data Stream a = a :& Stream a


instance P.Show a => P.Show (Stream a) where
  show xs = showInfinity (P.show (take 5 xs)) where
    showInfinity x = P.init x ++ "..."


constStream :: a -> Stream a
constStream a = iterate id a


head :: Stream a -> a
head (x:&_) = x


tail :: Stream a -> Stream a
tail (_:&xs) = xs


(!!) :: Stream a -> P.Int -> a
(!!) stream n = (getElement stream 0) where
  getElement (x:&xs) k | k < n     = getElement xs (n + 1)
                       | otherwise = x


take :: P.Int -> Stream a -> [a]
take 0 stream = []
take n stream = (head stream) : (take (n - 1) (tail stream))


map :: (a -> b) -> Stream a -> Stream b
map f (x:&xs) = (f x) :& (map f xs)


filter :: (a -> P.Bool) -> Stream a -> Stream a
filter f (x:&xs) | (f x)     = x :& (filter f xs)
                 | otherwise = filter f xs


zip :: Stream a -> Stream b -> Stream (a, b)
zip (x:&xs) (y:&ys) = (x, y) :& (zip xs ys)


zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (x:&xs) (y:&ys) = (f x y) :& (zipWith f xs ys)


iterate :: (a -> a) -> a -> Stream a
iterate f a = a :& (iterate f (f a))

