module Monads.Writer (
  Writer(..),
  runWriter
) where


import Prelude hiding ((*>), Writer(..))
import Monads.Kleisli
  
  
data Writer msg b = Writer (b, msg)
  

instance Functor (Writer msg) where
    fmap f w = Writer (f a, w1) where (a, w1) = runWriter w
  

instance Monoid msg => Applicative (Writer msg) where
    pure x  = Writer (x, mempty)
    fw <*> w = let (f, w1) = runWriter fw
                   (a, w2) = runWriter w
               in Writer (f a, mappend w1 w2)


instance Monoid msg => Monad (Writer msg) where
    return v = Writer (v, mempty)
    w >>= g = let (a1, w1) = runWriter w
                  (a2, w2) = runWriter $ g a1
              in Writer (a2, mappend w1 w2)


instance Monoid msg => Kleisli (Writer msg) where
    idK x = Writer (x, mempty)
    (*>) f g v = let (v1, w1) = runWriter $ f v
                     (v2, w2) = runWriter $ g v1
                 in Writer (v2, mappend w1 w2)


runWriter :: Writer msg b -> (b, msg)
runWriter (Writer a) = a