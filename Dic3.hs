module Dic3 where

import Data.Char (ord, toLower)
import Tree

data AbsD a = MkAbsD {comp :: (a->a->Bool), magn :: (a->Int)}

-- | Returns True the two elements have equal magnitude
(=||=) :: AbsD a -> (a -> a -> Bool)
(=||=) = comp

-- | Get the magnitude of this element as an Int
magnitude :: AbsD a  -> (a -> Int)
magnitude = magn

-- | Dictionary for Int
dAbsInt :: AbsD Int
dAbsInt = MkAbsD {comp = \x -> \y -> abs x == abs y, magn = abs }

-- | Dictionary for Int
dAbsChar :: AbsD Char
dAbsChar = MkAbsD {comp = \x -> \y -> toLower x == toLower y, magn = ord . toLower }

-- | Dictionary for Trees
dAbsTree :: AbsD (Tree a)
dAbsTree = error "Task 4" 

-- | Dictionary for Tree Int
dAbsTreeInt :: AbsD (Tree Int)
dAbsTreeInt = error "Task 4"

cmp :: AbsD a -> a -> a -> String
cmp dic x y = if((=||=) dic x y) then "Abs-Equal" else "Abs-Not-Equal"

cmpMagnitude :: AbsD a -> AbsD b -> a -> b -> String
cmpMagnitude dicx dicy x y = if(magnitude dicx x == magnitude dicy y) then "Abs-Equal" else "Abs-Not-Equal"
