
module TAPF.Typeclasses2 where

import           Prelude hiding (Num, sum)

-- simple typeclass for numbers
class Num a where
  fromInt :: Int -> a
  add :: a -> a -> a

instance Num Int where
  fromInt = id
  add     = (Prelude.+)

instance Num Float where
  fromInt = Prelude.fromIntegral
  add     = (Prelude.+)


-- example overloaded functions
twice :: Num a => a -> a
twice x = add x x

sum :: Num a => [a] -> a
sum []     = fromInt 0
sum (x:xs) = x `add` sum xs


--------------------------------------------------------

-- dictionary translation
data NumD a = NumD { fromInt_ :: Int -> a
                   , add_ :: a -> a -> a
                   }

-- an instance for Int
numDInt :: NumD Int
numDInt = NumD { fromInt_ = id
               , add_ = (Prelude.+)
               }


numDFloat :: NumD Float
numDFloat = NumD {  fromInt_ = Prelude.fromIntegral
                 ,  add_ = (Prelude.+)
                 }

twice' :: NumD a -> a -> a
twice' numD x = add_ numD x x

sum' :: NumD a -> [a] -> a
sum' numD []     = fromInt_ numD 0
sum' numD (x:xs) = add_ numD x (sum' numD xs)
