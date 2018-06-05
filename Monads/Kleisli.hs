module Monads.Kleisli (
    Category(..),
    Kleisli(..)  
) where


import Prelude hiding(id, (>>), (*>))

infixr 0 +$, *$


class Category cat where
    id   :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

     
class Kleisli m where
    idK  :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)


instance Category (->) where
    id x   = x
    f >> g = g . f


instance Kleisli Maybe where
    idK = Just
    f *> g = \a -> case f a of
                        Nothing -> Nothing
                        Just b  -> g b


(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)


(*$) :: Kleisli m => (a -> m b) -> m a -> m b
f *$ a = (const a *> f) ()


(+$) :: Kleisli m => (a -> b) -> m a -> m b
f +$ a = (const a +> f) ()
