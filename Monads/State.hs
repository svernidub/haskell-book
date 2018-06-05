module Monads.State (
    State(..),
    runState
) where

import Prelude hiding(State(..))
import Monads.Kleisli
    

data State s a = State (s -> (a, s))


instance Functor (State s) where
    fmap f s = State $ \s0 -> 
                        let (a, s1) = runState s s0
                        in (f a, s1)
                              

instance Applicative (State s) where
    pure x = State $ \s0 -> (x, s0)
    fstate <*> state = State $ \s0 ->
                            let (f, s1) = runState fstate s0
                                (a, st2) = runState state s1
                            in (f a, st2)


instance Monad (State s) where
    return v     = State $ \s -> (v, s)
    state >>= g  = State $ \s0 ->   let (v1, s1) = runState state s0
                                        (v2, s2) = runState (g v1) s1
                                    in (v2, s2)


instance Kleisli (State s) where
    idK a = State $ \s -> (a, s)
    (*>) f g v = State $ \s0 ->
                            let (v1, s1) = runState (f v) s0
                                (v2, s2) = runState (g v1) s1
                            in (v2, s2)
    

runState :: State s a -> s -> (a, s)
runState (State f) = f