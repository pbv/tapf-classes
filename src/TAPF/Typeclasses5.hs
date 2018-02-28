{-
  Simple tautology checker using typeclass instances
  Pedro Vasconcelos, 2017

  Try e.g.
  > check (\p -> p || not p)
  > check (\p -> p `implies` p)
  > check (\p q -> (p&&q) `implies` p)
  > check (\p q -> p `implies` (p&&q))
-}
{-# LANGUAGE FlexibleInstances #-}

module TAPF.Typeclasses5 where

class Testable prop where
  check :: prop -> Bool

instance Testable Bool where
  check b = b

instance Testable a => Testable (Bool -> a) where
  check f = check (f False) && check (f True)

-- helper function: boolean implication 
implies :: Bool -> Bool -> Bool
p `implies` q = not p || q
