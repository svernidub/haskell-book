module Stream where

import Prelude ((+), (-), (<), (++), (*),
               otherwise, id, negate, abs, signum, fromInteger)
import qualified Prelude as P

infixr 1 :&


data Stream a = a :& Stream a


instance P.Show a => P.Show (Stream a) where
  show xs = showInfinity (P.show (take 5 xs)) where
    showInfinity xs = P.init xs ++ "..."


instance P.Num a => P.Num (Stream a) where
  (+)         xs ys  = zipWith (+) xs ys
  (-)         xs ys  = zipWith (-) xs ys
  (*)         xs ys  = zipWith (*) xs ys
  negate      xs     = map negate xs
  abs         xs     = map abs xs
  signum      xs     = map signum xs
  fromInteger x      = P.error "Not implemented"


constStream :: a -> Stream a
constStream a = iterate id a


head :: Stream a -> a
head (x:&_) = x


tail :: Stream a -> Stream a
tail (_:&xs) = xs


(!!) :: Stream a -> P.Int -> a
(!!) xs 0 = head xs
(!!) xs n = (tail xs) !! (n - 1)


take :: P.Int -> Stream a -> [a]
take 0 xs = []
take n xs = (head xs) : (take (n - 1) (tail xs))


map :: (a -> b) -> Stream a -> Stream b
map f xs = f (head xs) :& map f (tail xs)


filter :: (a -> P.Bool) -> Stream a -> Stream a
filter f xs | f (head xs) = head xs :& filter f (tail xs)
            | otherwise   = filter f (tail xs)


zip :: Stream a -> Stream b -> Stream (a, b)
zip xs ys = (head xs, head ys) :& zip (tail xs) (tail ys)


zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f xs ys = f (head xs) (head ys) :& zipWith f (tail xs) (tail ys)


iterate :: (a -> a) -> a -> Stream a
iterate f a = a :& iterate f (f a)
