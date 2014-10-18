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

cmp :: Abs n => n -> n -> String
cmp x y = if (x =||= y) then "Abs-Equal" else "Abs-Not-Equal"

cmpMagnitude :: (Abs n, Abs m) => n -> m -> String
cmpMagnitude x y = if (magnitude x == magnitude y) 
                   then "Abs-Equal"
                   else "Abs-Not-Equal"


-- | Implemented the Abs instance definition for
-- arbitrary Tree's. A tree is equal (=||=) to another if it is
-- structually equivalent and all the leaf and node values are equal
-- (=||=).
instance Abs a => Abs (Tree a) where
 (Leaf x) =||= (Leaf y) = x =||= y
 (Node x lx rx) =||= (Node y ly ry) = (x =||= y) || (lx =||= ly) || (rx =||= ry)
 _ =||= _ = False

 magnitude (Leaf x) = magnitude x
 magnitude (Node x lx rx) = (magnitude x) * height (Node x lx rx)
