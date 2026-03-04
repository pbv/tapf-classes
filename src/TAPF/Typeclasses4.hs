
module TAPF.Typeclasses4 where

import           Prelude hiding (Show, show, showList)
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
  show ys = "[" ++ showList ys ++ "]"
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

showDPair :: ShowD a -> ShowD b -> ShowD (a,b)
showDPair d1 d2
  = ShowD { show_ = \(a,b) -> "("++show_ d1 a ++ "," ++ show_ d2 b ++")"}



-- An example that shows that it may be impossible
-- to statically know every type that a polymorphic function is used at.
-- Requires polymorphic recursion so type signature is mandatory
nested :: Show a => Int -> a -> String
nested 0 x  = show x
nested n x  = nested (n-1) (x,x)

-- This IO action will nest a character `n' times
-- where n is read from stdin; hence the actual runtime type
-- will not be known statically
test :: IO ()
test = do
  n <- readLn
  putStrLn (nested n 'A')
  
-- In the translation the dictionary is computed at runtime;
-- there is no way to monomorphize this
nested' :: ShowD a -> Int -> a -> String
nested' d 0 x = show_ d x
nested' d n x = nested' (showDPair d d) (n-1) (x,x)

test' :: IO ()
test' = do
  n <- readLn
  putStrLn (nested' showDChar n 'A')
