module Monads.Reader (
    Reader(..),
    runReader,
    (*>)
) where

import Prelude hiding(Reader(..), (*>))
import Monads.Kleisli

data Reader env b = Reader (env -> b)


instance Functor (Reader env) where
    fmap f e = Reader $ \env -> f $ runReader e env


instance Applicative (Reader env) where
    pure x       = Reader $ \env -> x
    fenv <*> e   = Reader $ \env ->
                            let f  = runReader fenv env
                                a = runReader e env
                            in f a

instance Monad (Reader e) where
    return x = Reader $ \env -> x
    e >>= g  = Reader $ \env ->
                            let x = runReader e env
                            in runReader (g x) env

instance Kleisli (Reader env) where
    idK x = Reader $ \env -> x
    (*>) f g x = Reader $ \env ->
                            let x' = runReader (f x) env
                            in runReader (g x') env


runReader :: Reader env b -> (env -> b)
runReader (Reader f) = f


