
module TAPF.Typeclasses1 where

import           Prelude hiding (Show, show, print)
import qualified Prelude (show)

-- a simple typeclass for converting to string
class Show a where
   show :: a -> String

-- two instances
instance Show Bool where
   show b = if b then "True" else "False"

instance Show Int where
  show = Prelude.show  -- Prelude implementation

-- the dictionary translation  
data ShowD a = ShowD { show_ :: a -> String }

showDBool :: ShowD Bool
showDBool = ShowD { show_ = \b -> if b then "True" else "False" }
            
showDInt :: ShowD Int
showDInt = ShowD { show_ = Prelude.show -- Prelude implementation
                 }


-- overloaded printing function
-- note that `print' is overloaded but `putStrLn' is not:
-- putStrLn :: String -> IO ()

print :: Show a => a -> IO ()
print x = putStrLn (show x)

-- dictionary passing version of the above
print' :: ShowD a -> a -> IO ()
print' showD x = putStrLn (show_ showD x) 


print2 :: (Show a, Show b) => (a,b) -> IO ()
print2 (x,y) = putStrLn (show x ++ show y)

print2' :: (ShowD a, ShowD b) -> (a,b) -> IO ()
print2' (dic1, dic2) (x,y)
  = putStrLn ((show_ dic1 x) ++ (show_ dic2 y))

