module Lab3 where

import Data.Char (ord, toLower)
import Tree

class Abs a where
 -- | Returns True the two elements are abs-equal
 (=||=) :: a -> a -> Bool 
 -- | Get the integral "magnitude" of this element
 magnitude :: a -> Int

instance Abs Int where
  x =||= y   = abs x == abs y
  magnitude  = abs

instance Abs Char where
  x =||= y   = toLower x == toLower y
  magnitude  = ord . toLower

cmp :: Abs n => n -> n -> Bool
cmp x y = x =||= y

cmpMagnitude :: (Abs n, Abs m) => n -> m -> Bool
cmpMagnitude x y = magnitude x == magnitude y


-- | Implemented the Abs instance definition for
-- arbitrary Tree's. A tree is equal (=||=) to another if it is
-- structually equivalent and all the leaf and node values are equal
-- (=||=).
instance Abs (Tree a) where
 (=||=) = error "Task 1"

 magnitude = error "Task 1"
