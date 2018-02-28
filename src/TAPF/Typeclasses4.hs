
module TAPF.Typeclasses4 where

import           Prelude hiding (Show, show, print)
import qualified Prelude (show)

-- a simple typeclass for converting to string
class Show a where
   show :: a -> String

-- instances for integers and lists
instance Show Int where
  show = Prelude.show  -- default implementation

instance Show Char where
  show = Prelude.show
  
instance Show Bool where
  show b = if b then "True" else "False"

instance Show a => Show [a] where
  show xs = "[" ++ showList xs ++ "]"
    where showList [] = ""
          showList [x] = show x
          showList (x:xs) = show x ++ "," ++ showList xs


instance (Show a, Show b)  => Show (a,b) where
  show (x,y) = "(" ++ show x ++ "," ++ show y ++ ")"
-----------------------------------------------------------------------
-- dictionary translation
-----------------------------------------------------------------------
data ShowD a = ShowD { show_ :: a -> String }

showDInt :: ShowD Int
showDInt = ShowD { show_ = Prelude.show -- Prelude implementation
                 }

showDChar :: ShowD Char
showDChar = ShowD { show_ = Prelude.show -- Prelude implementation
                  }


showDList :: ShowD a -> ShowD [a]
showDList d
  = ShowD { show_ = \xs -> "[" ++ showList xs ++ "]" }
  where showList [] = ""
        showList [x] = show_ d x
        showList (x:xs) = show_ d x ++ "," ++ showList xs



-- this function requires a type signature
-- (polymorphic recursion)
nested :: Show a => Int -> a -> String
nested 0 x       = show x
nested n x | n>0 = nested (n-1) (x,x)


-- testing
test :: IO ()
test = do
  n <- getLine
  putStrLn (nested (read n) 'A')
  

-- dictionary translation
nested' :: ShowD a -> Int -> a -> String
nested' d 0 x       = show_ d x
nested' d n x | n>0 = nested' (showDList d) (n-1) (replicate n x)

test' :: IO ()
test' = do
  n <- getLine
  putStrLn (nested' showDChar (read n) 'A')
