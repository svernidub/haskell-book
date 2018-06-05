module Monads.Writer (
  Writer(..),
  runWriter
) where


import Prelude hiding ((*>), Writer(..))
import Monads.Kleisli
  
  
data Writer msg b = Writer (b, msg)
  

instance Functor (Writer msg) where
    fmap f w = Writer (f a, w1) where (a, w1) = runWriter w
  

instance Applicative (Writer msg) where
    pure x  = error ""
    fw <*> w = let (f, w1) = runWriter fw
                   (a, w2) = runWriter w
               in Writer (f a, w2)


instance Monad (Writer msg) where
    return v = error ""
    w >>= g = g a where (a, w1) = runWriter w


instance Kleisli (Writer msg) where
    idK x = error ""
    (*>) f g v = g v1 where (v1, w1) = runWriter (f v)


runWriter :: Writer msg b -> (b, msg)
runWriter (Writer a) = a