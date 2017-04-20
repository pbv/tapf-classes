{-
  Simple tautology checker using typeclass instances
  Pedro Vasconcelos, 2017

  Try e.g.
  > taut (\p -> p || not p)
  > taut (\p -> p `implies` p)
  > taut (\p q -> (p&&q) `implies` p)
  > taut (\p q -> p `implies` (p&&q))
-}
{-# LANGUAGE FlexibleInstances #-}

module TAPF.Tautology where

class Testable prop where
  taut :: prop -> Bool

instance Testable Bool where
  taut b = b

instance Testable a => Testable (Bool -> a) where
  taut f = taut (f False) && taut (f True)


-- helper function: boolean implication 
implies :: Bool -> Bool -> Bool
p `implies` q = not p || q
