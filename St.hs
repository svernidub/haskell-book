{-# LANGUAGE NoImplicitPrelude #-}

module St where

import qualified GHC.Base(map)
import qualified Data.Tuple(fst)
import Control.Category(Category(..))


data St a b = St (a -> (b, St a b))


ap :: St a b -> [a] -> [b]
ap (St func) xs = map fst (map func xs) where
                      map = GHC.Base.map
                      fst = Data.Tuple.fst


instance Category St where
    id                  = St empty where
                              empty x = (x, St empty)

    (.) (St f2) (St f1) = St apply where
        apply x = let (y, st1) = f1 x
                      (z, st2) = f2 y in
                  (z, St apply)
