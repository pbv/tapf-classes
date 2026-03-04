
module TAPF.Typeclasses1 where

import           Prelude hiding (Show, show)
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


-- overloaded function
greet :: Show a => a -> String
greet x = "Hello " ++ show x

-- dictionary passing version of the above
greet' :: ShowD a -> a -> String
greet' showD x = "Hello " ++ show_ showD x


