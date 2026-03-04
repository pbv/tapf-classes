
module TAPF.Typeclasses2 where

import           Prelude hiding (Num, sum)

-- simple typeclass for numbers
class Num a where
  fromInt :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Num Int where
  fromInt = id
  add     = (Prelude.+)
  mul     = (Prelude.*)

instance Num Float where
  fromInt = Prelude.fromIntegral
  add     = (Prelude.+)
  mul     = (Prelude.*)


-- example overloaded functions
square :: Num a => a -> a
square x = mul x x

incr :: Num a => a -> a
incr x = add x (fromInt 1)

sum :: Num a => [a] -> a
sum []     = fromInt 0
sum (x:xs) = x `add` sum xs


--------------------------------------------------------

-- dictionary translation
data NumD a = NumD { fromInt_ :: Int -> a
                   , add_ :: a -> a -> a
                   , mul_ :: a -> a -> a
                   }

-- instance for Int
numDInt :: NumD Int
numDInt = NumD { fromInt_ = id
               , add_ = (Prelude.+)
               , mul_ = (Prelude.*)
               }


numDFloat :: NumD Float
numDFloat = NumD { fromInt_ = Prelude.fromIntegral
                 , add_ = (Prelude.+)
                 , mul_ = (Prelude.*)
                 }

square' :: NumD a -> a -> a
square' numD x = mul_ numD x x

incr' :: NumD a -> a -> a
incr' numD x = add_ numD x (fromInt_ numD 1)

sum' :: NumD a -> [a] -> a
sum' numD []     = fromInt_ numD 0
sum' numD (x:xs) = add_ numD x (sum' numD xs)
