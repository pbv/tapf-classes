
module TAPF.Typeclasses3 where

import qualified Prelude ((==))
import           Prelude hiding (Eq, (==))


-- simplified version of the equality class
class Eq a where
  eq :: a -> a -> Bool

-- some instances
instance Eq Bool where
  eq x y = if x then y else not y
  
instance Eq Int where
  eq = (Prelude.==)


-- an overloaded function
member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (y:ys) = x`eq`y  || member x ys


----------------------------------------------------------------
-- dictionary translation
-----------------------------------------------------------------

data EqD a = EqD { eq_ :: a -> a -> Bool }

eqDInt :: EqD Int
eqDInt = EqD (Prelude.==)

eqDBool :: EqD Bool
eqDBool = EqD (\x y -> if x then y else not y)


member' :: EqD a -> a -> [a] -> Bool
member' eqD x [] = False
member' eqD x (y:ys) = eq_ eqD x y || member' eqD x ys



-- translating an instance with constraints e.g. pairs and lists
instance (Eq a, Eq b) => Eq (a,b) where
  eq (x,y) (x',y') = eq x x' && eq y y'


eqDPair :: (EqD a, EqD b) -> EqD (a,b)
eqDPair (EqD eqA, EqD eqB)
  = EqD (\(x,y) (x',y') -> eqA x x' && eqB y y') 


instance Eq a => Eq [a] where
  eq [] []         = True
  eq (x:xs) (y:ys) = eq x y && eq xs ys
  eq _      _      = False


eqDList :: EqD a -> EqD [a]
eqDList (EqD eqA) = EqD eqL
  where
    eqL [] []         = True
    eqL (x:xs) (y:ys) = eqA x y && eqL xs ys
    eqL _      _      = False







