module Nat where

data Nat = Zero | Succ Nat
     deriving (Show, Eq, Ord)

instance Num Nat where
  (+) a Zero     = a
  (+) a (Succ b) = Succ (a + b)

  negate _ = error "negate is undefined for Nat"

  (*) a Zero     = Zero
  (*) a (Succ b) = a + (a * b)

  abs x = x

  signum x    = x
  signum Zero = Zero
  signum _    = Succ Zero
  fromInteger 0 = Zero
  fromInteger n = Succ (fromInteger (n - 1))

beside :: Nat -> Nat -> Bool
beside a b = (Succ a) == b || a == (Succ b)

beside2 :: Nat -> Nat -> Bool
beside2 a b = do
  (Succ (Succ a)) == b || a  == (Succ (Succ b))
